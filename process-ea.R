# This process attempts to reconcile the existing src/main/rlocale_data.h with
# the Unicode EastAsianWidth Annex 11 file from
# http://www.unicode.org/reports/tr11/
#
# The rlocale file contains addditional locale-specific width interpretations
# that the unicode data does not have.  For this reason we never let the unicode
# data override anything but the first column of the rlocale data.  Additionally
# rlocale_data.h has special ifdef handling for windows.  Most of the complexity
# in the code that follows below is to preserver the extra locale and ifdef
# data.
#
# See also 'process-zw.R' for Zero width, and 'process-lib.R' for other "hard
# coded" assumptions that affect width interpretation:

# - Parse Unicode EAW ----------------------------------------------------------

# Read the EastAsianWidth.txt table into a data.frame:

source('process-lib.R')

udat <- uni_eaw("EastAsianWidth.txt")

# We should probably collapse ranges that are contiguous and have the
# same width property as we are not currently using any of the other
# properties that cause ranges to be split.

ldat <- parse_rlocale('rlocale_data.h')

# originally the stuff in ldat was generated in the script and
# was available to rest of script, hence the list2env horror below as
# I don't want to go through and update all the code:

list2env(ldat, environment())

# - Map R Entries to Unicode ---------------------------------------------------

# Generate a mapping of R's entries to unicode EAW entries; need to
# handle fact there are duplicate R entries due to ifdef statements

rallp <- all_points(rdat, 'r')
uallp <- all_points(udat, 'u')
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

# Drop the zero width entries, as some of these weirdly have a wide EAW
# representation that would override the subsequent zero-width step

map <- map[
  !uall[match(map[['id']], uall[['V1']]), 'V3'] %in% ZW_GC |
  map[['id']] %in% ZW_EXCLUDE_CP  # soft-hyphen
  ,
]
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
map.c[['eaw']] <- udat[map.c[['uid']], 'V2']

# Map EAW to width

map.c[['W']] <- EAW[map.c[['eaw']]]
stopifnot(!anyNA(map.c[['EAW']]), !anyNA(map.c[['W']]))

# If no rid, set all to new W value, otherwise update the first element
# to the new value, and preserve the other locale-specific data

map.c[!rid.val, wcols] <- map.c[!rid.val, 'W']
map.c[rid.val, 'X1'] <- map.c[rid.val, 'W']

map.c[rid.val, 'comment'] <- rdat[map.c[['rid']][rid.val], 'comment']
map.c[rid.val, 'rid_raw'] <- rdat[map.c[['rid']][rid.val], 'rid_raw']

# Drop all new uid mappings that have width == 1
# as that is the unspecified width (it may be useful for testing to leave
# these in as they may reveal problems with the mapping code)

map.c <- map.c[rid.val | map.c[['W']] != 1L,]
rm(rid.val)  # not valid anymore

# - Recreate Text Entries ------------------------------------------------------

# We need to regenerate the rlocale_data.h format, recovering previously
# existing comments, ifdefs, etc.

# Collapse sequential entries that are otherwise identical
# Possibly a lot of this complexity could have been avoided
# by pre-collapsing the unicode table?  Lots of distinctions
# there that end up being irrelevant for this table.
#
# * Every entry is the same
# * if/def break ranges
# * Only one distinct comment at top of group

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
# immediately behind the previous rlocale row

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

# Interleave with non data rows (ifdefs/comments)

txtres <- rep(txt1, reps)
txtresb <- rep(seq_along(txt1) %in% lbase, reps)
txtres[txtresb] <- txt.raw

# We will give each ifdef block as start point the lowest start
# in the block.  This will allow us to reorder everything according
# to start without mixing ifdef els with non-ifdef

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

# - Sanity Checks --------------------------------------------------------------

# * Everything in order for each ifdef mode
# * No codepoint that was specified in rdat missing
# * The two ifdef modes are essentially the same

ldat2 <- parse_rlocale('rlocale_data2.h')
rdat2 <- ldat2[['rdat']]
# ifdef TRUE
rdat2a <- subset(rdat2,
  rid_raw %in% c(ldat2[['lwin']] + 1L) | (!rid_raw %in% ldat2[['isifdef']])
)
# ifdef FALSE
rdat2b <- subset(rdat2,
  rid_raw %in% c(ldat2[['lwin']] + 3L) | (!rid_raw %in% ldat2[['isifdef']])
)

with(rdat2a,
  stopifnot(
    all(start[-1] > start[-length(start)]),
    all(end[-1] > end[-length(end)]),
    all(start[-1L] > end[-length(end)])
) )
with(rdat2b,
  stopifnot(
    all(start[-1] > start[-length(start)]),
    all(end[-1] > end[-length(end)]),
    all(start[-1L] > end[-length(end)])
) )
rallp2a <- all_points(rdat2a, 'r')
rallp2b <- all_points(rdat2b, 'r')
stopifnot(
  identical(rallp2a$id, rallp2b$id),
  all(rallp$id %in% rallp2a$id)
)
