# - Parse Unicode EAW ----------------------------------------------------------

# Read the EastAsianWidth.txt table into a data.frame (From Gábor),
# first part:

options(stringsAsFactors=FALSE)
udat <- read.delim(
  stringsAsFactors = FALSE,
  "EastAsianWidth.txt",
  comment.char = "#",
  sep = ";",
  strip.white = TRUE,
  header = FALSE
)
# Generate and validate the ranges

points <- strsplit(udat[['V1']], '..', fixed=TRUE)
lens <- lengths(points)
stopifnot(all(lens > 0 & lens < 3))
for(i in seq_along(points)) length(points[[i]]) <- 2
points <- matrix(unlist(points), 2)

udat[['start']] <- points[1,]
udat[['end']] <- points[2,]
udat <- within(udat, end[is.na(end)] <- start[is.na(end)])
udat[c('start', 'end')] <- lapply(udat[c('start', 'end')], as.hexmode)

# we rely on EastAsianWidth correctly representing emoji presentation

eaw <- c(N=1L, Na=1L, W=2L, F=2L, H=1L, A=1L)

with(udat,
  stopifnot(
    all(V2 %in% names(eaw)),
    all(start <= end),
    all(diff(start) > 0),
    all(diff(end) > 0)
  )
)
# We should probably collapse ranges that are contiguous and have the
# same width property as we are not currently using any of the other
# elements that cause ranges to be split.

# - Parse R EAW ----------------------------------------------------------------

# Parse the R data that exists, look for `table_wcwidth` and the next table
# after that, and process the data in between

raw <- readLines('rlocale_data.h')
rlstr <- grep('static +const +struct +interval_wcwidth +table_wcwidth\\[\\]', raw)
rlend <- grep('static +const +struct +interval +table_walpha\\[\\]', raw)
stopifnot(length(rlend) == 1, length(rlstr) == 1, rlstr + 1 < rlend)
txt1 <- raw[(rlstr + 1):rlend]

# Either we have a data row, or we have a conditional row, or a comment row

pbase <- paste0(
  "^\\s*\\{0x([0-9A-Fa-f]{2,6}),0x([0-9A-Fa-f]{2,6})(,\\{[012](?:,[012]){6}\\})\\},",
  "(\\s*(?://.*|/\\*.*\\*/|))\\s*$"
)
pwin <- "^#ifdef\\s*Win32"
pwinelse <- "^#else\\s*"
pwinend <- "^#endif\\s*"
pcomm <- "^\\s*//"

lbase <- grep(pbase, txt1)
lwin <- grep(pwin, txt1)
lwinelse <- grep(pwinelse, txt1)
lwinend <- grep(pwinend, txt1)
lcomm <- grep(pcomm, txt1)

# We should have matched every line in the table with these patterns

stopifnot(
  all.equal(
    sort(c(lbase, lwin, lwinelse, lwinend, lcomm)),
    seq(1L, rlend - rlstr - 5, by=1)
  ),
  txt1[rlend - rlstr - 4] == "};"
)
# Extract data into columns, we'll have duplicate ranges because
# of the ifdefs; that should be fine.

txt2 <- txt1[lbase]
matches <- regexec(pbase, txt2, perl=TRUE)
txt3 <- regmatches(txt2, matches)

stopifnot(all(lengths(txt3) == 5))

rdat <- as.data.frame(t(matrix(unlist(txt3), 5)[2:5,]), stringsAsFactors=FALSE)
names(rdat) <- c('start', 'end', 'widths', 'comment')

wm <- gregexpr("[0-9]+", rdat$widths)
wi <- regmatches(rdat$widths, wm)

stopifnot(all(lengths(wi) == 7))
wil <- data.frame(
  matrix(unlist(wi), ncol=7, byrow=TRUE), stringsAsFactors=FALSE
)
rdat <- cbind(rdat, wil)
rdat[c('start', 'end')] <- lapply(rdat[c('start', 'end')], as.hexmode)
rdat[['rid_raw']] <- lbase

with(rdat,
  stopifnot(
    all(start <= end),
    # duplicates possible due of ifdefs producing different tables for
    # Win32...
    all(diff(as.integer(start)) >= 0),
    all(diff(as.integer(end)) >= 0)
  )
)
# - Map R Entries to Unicode ---------------------------------------------------

# Generate a mapping of R's entries to unicode entries; need to
# handle fact there are duplicate R entries due to windows

allpoints <- seq(0L, max(c(udat[['end']], rdat[['end']])))
rsizes <- with(rdat, end - start + 1L)
rallp <- with(
  rdat,
  data.frame(
    id=rep(start - 1L, rsizes) + sequence(rsizes),
    rid=rep(seq_len(nrow(rdat)), rsizes)
) )
usizes <- with(udat, end - start + 1L)
uallp <- with(
  udat,
  data.frame(
    id=rep(start - 1L, usizes) + sequence(usizes),
    uid=rep(seq_len(nrow(udat)), usizes)
) )
map <- merge(rallp, uallp, by='id', all=TRUE)
with(
  map, stopifnot(
    !anyNA(uid),
    all(uallp[['uid']] %in% uid),
    all(rallp[['rid']] %in% rid[!is.na(rid)]),
    !is.unsorted(id)
) )
map <- transform(map, rid=ifelse(is.na(rid), -1L, rid))
map <- transform(map, urid=cumsum(c(0L, (diff(uid) != 0) | (diff(rid) != 0))))

# Collapse back to ranges for each uid/rid interaction, ignoring stuff less than
# 0xa1 as R doesn't seem to care about that, and also get rid of all the -1
# entries from unicode (i.e. no EAW spec)

map <- subset(map, id > 0xa0 & uid > -1L)
map.s <- split(map, map[['urid']])
map.c <- as.data.frame(
  t(
    vapply(
    map.s,
    function(x) c(range(x[[1]]), x[[3L]][1L], x[[4L]][1L]),
    integer(4)
) ) )
names(map.c) <- c('start', 'end', 'uid', 'rid')
map.c[c('start', 'end')] <- lapply(map.c[c('start', 'end')], as.hexmode)

# Add the R width specs, starting with default to wide for everything, and
# add the EAW attribute

wcols <- paste0('X', 1:7)
map.c <- cbind(
  map.c,
  as.data.frame(setNames(as.list(rep(2L, 7L)), paste0('X', 1:7)))
)
rid.val <- map.c$rid >= 0
map.c[rid.val, wcols] <- rdat[map.c[['rid']][rid.val], wcols]
map.c[['EAW']] <- udat[map.c[['uid']], 'V2']

# Map EAW to width

map.c[['W']] <- eaw[map.c[['EAW']]]
stopifnot(!anyNA(map.c[['EAW']]), !anyNA(map.c[['W']]))

# If no rid, set all to new W value, otherwise update the first element
# to the new value, and add auxillary daata

map.c[!rid.val, wcols] <- map.c[!rid.val, 'W']
map.c[rid.val, 'X1'] <- map.c[rid.val, 'W']

map.c[rid.val, 'comment'] <- rdat[map.c[['rid']][rid.val], 'comment']
map.c[rid.val, 'rid_raw'] <- rdat[map.c[['rid']][rid.val], 'rid_raw']

# - Recreate Text Entries ------------------------------------------------------

# Collapse sequential entries that are otherwise identical
# Possibly a lot of this complexity could have been avoided
# by pre-collapsing the unicode table?  Lots of distinctions
# there that end up being irrelevant for this table.
#
# * Every entry is the same
# * No if/def (can test sequential rid_raw for this)
# * Only one distinct comment per group

map.t <- with(map.c,
  data.frame(
    start=c(start), end=c(end),
    uid, rid,
    widths=sprintf(
      ',{%s}', do.call(paste, c(unname(map.c[wcols]), list(sep=',')))
    ),
    comment=ifelse(is.na(comment), "", comment),
    rid_raw=ifelse(is.na(rid_raw), -1L, rid_raw)
) )
# Assign group ids

map.t <- within(map.t,{
  se <-c(start[-1] - end[-length(end)] == 1, FALSE)
  we <-c(widths[-1] == widths[-length(widths)], FALSE)
  re <-c(diff(rid_raw) == 1L | diff(rid_raw) == 0L, FALSE)
  b1 <- b2 <- !(se & we & re)
  # groups start and end one later than the switch in values
  # is reported in (because we use cumsum below)
  b2[which(diff(b1) == -1) + 1] <- TRUE
  b2[which(diff(b1) == +1) + 1] <- FALSE
  group <- cumsum(b2)
  rm(se,we,re,b1,b2)
})
# Check that in groups there is at most one distinct non-na, non empty
# comment, and the group has multiple rows

f <- function(x) length(x) > 1 && sum(nzchar(x)) < 2
collapse <- with(map.t, tapply(comment, group, f))
group.multi <- as.integer(names(which(collapse)))
map.tc <- subset(map.t, group %in% group.multi)

# Collapse the groups

map.tc.rle <- rle(map.tc$group)
map.tc.id <- matrix(cumsum(rbind(1, map.tc.rle$lengths - 1)), 2)
map.tcc <- map.tc[map.tc.id[1,],]
map.tcc[,'end'] <- map.tc[map.tc.id[2,],'end']

# Need comment, get it by putting longest comment first in
# each group (only one comment in this whole block?? - is that right)

map.com <- with(map.tc, comment[order(group, -nchar(comment))])
map.tcc['comment'] <- map.com[map.tc.id[1,]]

# Recombine with the single row groups

map.fin <- rbind(
  subset(map.t, !group %in% group.multi),
  map.tcc
)
map.fin <- map.fin[order(map.fin[['start']]),]
with(map.fin, stopifnot(all(start[-1] > end[-length(end)])))
map.fin[c('start', 'end')] <- lapply(map.fin[c('start', 'end')], as.hexmode)
map.fin[['id']] <- seq_len(nrow(map.fin))

# Map back to position in originial rlocale table; new rows will go
# immediately behind the closest rlocale row

map.fin[['rid2']] <- cummax(map.fin[['rid']])

# Need a target vector that has:
# * text
# * original position
# * new position
#
# So need to generate a text vector that can hold all the original entries
# plus the new ones.  Maybe repeat the old ones that now have groups?  What
# about collapsing?

# STILL NEED TO GET COMMENT

# Need output with as many rows as map.t + all the ifdef and comment rows
