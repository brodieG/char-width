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

# We should have matched every line in the table with these patterns,
# and ifdefs els should all be one apart from each other, all should
# be if/else/end, and none should be contiguous.  This allows us to
# make helpful assumptions later, such as we can't be adding a new
# unicode point inside an ifdef block.

stopifnot(
  all.equal(
    sort(c(lbase, lwin, lwinelse, lwinend, lcomm)),
    seq(1L, rlend - rlstr - 5, by=1)
  ),
  txt1[rlend - rlstr - 4] == "};",
  length(lwin) == length(lwinelse),
  length(lwin) == length(lwinend),
  all((lwinelse - lwin) == 2L),
  all((lwinend - lwinelse) == 2L)
)
isifdef <- rep(0:4, length(lwin)) + rep(lwin, each=5)

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

# urid is effectively the interaction of the uid and rid

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
    function(x) c(range(x[[1]]), x[['uid']][1L], x[['rid']][1L]),
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

# Drop all new uid mappings that have width == 1
# as that is the unspecified width

# map.c <- map.c[rid.val | map.c[['W']] != 1L,]
rm(rid.val)  # not valid anymore

# - Recreate Text Entries ------------------------------------------------------

# Collapse sequential entries that are otherwise identical
# Possibly a lot of this complexity could have been avoided
# by pre-collapsing the unicode table?  Lots of distinctions
# there that end up being irrelevant for this table.
#
# * Every entry is the same
# * No if/def (can test sequential rid_raw for this)
# * Only one distinct comment per group

rid.max <- max(rallp[['rid']])
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
map.t[c('start', 'end')] <- lapply(map.t[c('start', 'end')], as.hexmode)
# Generate proxy ids for the new rids.  We'll use the prior
# rid if there is one and it isn't part of an ifdef, or the
# next one if not.

ridmap <- subset(map.t[c('rid', 'rid_raw')], rid >= 0)
ridnew <- map.t[['rid']] < 0
ridtmp <- ifelse(ridnew, cummax(map.t[['rid']]), map.t[['rid']])

# check which are mapped to an ifdef block, for those, map to the
# next rid until we're definitely not in an ifdef block.  This means
# some rows will end up out of order, we'll need to reorder later

k <- 1
repeat {
  if((k <- k + 1) > 10) stop("too many adjacent ifdefs")
  newmap <- match(ridtmp[ridnew], ridmap[['rid']])
  hitifdef <- which(ridmap[['rid_raw']][newmap] %in% isifdef)
  if(!length(hitifdef)) break
  hitifdeftmp <- (ridtmp %in% ridmap[['rid']][newmap][hitifdef]) & ridnew
  ridtmp[hitifdeftmp] <- ridtmp[hitifdeftmp] + 1L
}
stopifnot(all(ridtmp <= rid.max))
map.t[['rid']] <- ridtmp

# recall there are possible duplicate ranges from ifdefs,
# ordering them by rid puts the duplicate entries together
# for re-collapse.

map.t <- map.t[with(map.t, order(rid, start, end)),]

# Assign group ids

map.t <- within(map.t,{
  se <-c(start[-1] - end[-length(end)] == 1, FALSE)
  we <-c(widths[-1] == widths[-length(widths)], FALSE)
  re <-c(diff(rid_raw) == 1L | diff(rid_raw) == 0L, FALSE)
  # new comment starts group
  cm <-c(
    (comment[-1] == comment[-length(comment)]) | !nzchar(comment[-1]),
    FALSE
  )
  b1 <- b2 <- !(se & we & re & cm)
  # groups start and end one later than the switch in values
  # is reported in (because we use cumsum below)
  b2[which(diff(b1) == -1) + 1] <- TRUE
  b2[which(diff(b1) == +1) + 1] <- FALSE
  group <- cumsum(b2)
  # rm(se,we,re,b1,b2)
})
f <- function(x) length(x) > 1
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

# Recombine with the single row groups, need to order by rid so
# final order for ifdefs works

map.fin <- rbind(
  subset(map.t, !group %in% group.multi),
  map.tcc
)
map.fin <- map.fin[with(map.fin, order(rid, start)),]
map.fin[c('start', 'end')] <- lapply(map.fin[c('start', 'end')], as.hexmode)
map.fin[['id']] <- seq_len(nrow(map.fin))

# Map back to position in originial rlocale table; new rows will go
# immediately behind the closest rlocale row

ridt <- table(map.fin[['rid']])
ridv <- as.integer(names(ridt))
rid.count <- table(map.fin[['rid']])
reps.raw <- integer(length(lbase))
reps.id <- as.integer(names(rid.count))
reps.raw[reps.id] <- rid.count
reps <- rep(1L, length(txt1))
reps[lbase] <- reps.raw

# Generate the new text

map.fin[map.fin[['rid_raw']] == -1L,'comment'] <- ' // unicode 12'
map.fin[c('start', 'end')] <- lapply(
  map.fin[c('start', 'end')], as.character, width=0
)
fin.mx <- do.call(rbind, map.fin[c('start', 'end', 'widths', 'comment')])
fin.dat <- lapply(seq_len(ncol(fin.mx)), function(x) fin.mx[,x])

txt.raw <- rep(txt2, reps.raw)
matches.proc <- lapply(
  matches, function(x) {
    y <- x[-1]
    attributes(y) <- attributes(x)
    attr(y, 'match.length') <- attr(x, 'match.length')[-1]
    y
  }
)
match.raw <- rep(matches.proc, reps.raw)
regmatches(txt.raw, match.raw) <- fin.dat

# Insert back into ifdefs

txtres <- rep(txt1, reps)
txtresb <- rep(seq_along(txt1) %in% lbase, reps)
txtres[txtresb] <- txt.raw

# Get start point for each ifdef block for final re-order,
# which will be the lowest start in block

lnres <- rep(seq_along(txt1), reps)
lnresifdef <- lnres %in% isifdef
lnmax <- max(as.hexmode(map.fin[['end']]))
lnstarts <- rep(lnmax + 1L, length(lnres))
lnstarts[txtresb] <- as.hexmode(map.fin[['start']])

lrdf.ids <- cumsum(lnres %in% lwin)
lnres.grp <- ave(
  lnstarts, as.integer(interaction(lrdf.ids, lnresifdef)), FUN=min
)
lnstarts[lnresifdef] <- lnres.grp[lnresifdef]
lnstarts[lnstarts > lnmax] <- 0L
lnstarts <- ifelse(lnstarts == 0L, cummax(lnstarts), lnstarts)

txtres <- txtres[order(lnstarts)]

# and into final file

txtfin <- c(raw[seq_len(rlstr)], txtres, raw[-seq_len(rlend)])
writeLines(txtfin, 'rlocale_data2.h')

# Need a target vector that has:
# * text
# * original position
# * new position
#
# So need to generate a text vector that can hold all the original entries
# plus the new ones.  Maybe repeat the old ones that now have groups?  What
# about collapsing?


# Need output with as many rows as map.t + all the ifdef and comment rows
