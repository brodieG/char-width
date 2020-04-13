# - Unicode Dat ----------------------------------------------------------------

options(stringsAsFactors=FALSE)
uall <- read.delim(
  stringsAsFactors = FALSE,
  "data/UnicodeData.txt",
  comment.char = "#",
  sep = ";",
  strip.white = TRUE,
  header = FALSE
)
uall[['V1']] <- as.hexmode(uall[['V1']])

# - EAW ------------------------------------------------------------------------

source('R/process-lib.R')
udat <- uni_eaw("data/EastAsianWidth.txt")
uallp <- all_points(udat, 'u')

# - Code Points ----------------------------------------------------------------

# Generate every code point from 0x1 to 0x10fffd; 0x0 causes problems so we skip
# it

start <- as.character(
  parse(
    text=paste0(
      "c(",
      paste0(
        "'\\u", as.character(as.hexmode(1:0xa0)), "'",
        collapse=",\n"
      ),
      ",NULL)"
    ),
    keep.source=FALSE
  )[[1]][-1]
)
start <- start[-length(start)]
end <- as.character(
  parse(
    text=paste0(
      "c(",
      paste0(
        "'\\U", as.character(as.hexmode(0xa1:0x10fffd), width=8), "'",
        collapse=",\n"
      ),
      ",NULL)"
    ),
    keep.source=FALSE
  )[[1]][-1]
)
end <- end[-length(end)]
cps <- c(start, end)

# - Compare --------------------------------------------------------------------

# Compare to other sources:
#
# * glibc 2.29                (Unicode 11?)
# * utf8(1.2.0)::utf8_width   (Unicode 12, but github only)
# * stri(1.4.6)::stri_width   (Unicode 10 - ICU4C 61.1)
# * R 3.6.3                   (Unicode 8ish)
# * R 4.0 r78107 w/ patch     (Unicode 12.1)
#
# glibc appears to return -1 when things are undefined, instead of 'Cn' like
# stringi.  The latter seems more correct.  UTF8 seems to return 6 or 10 for
# undefined code points, and possibly some others.

cpsa <- cps
cpsa[0xd800:0xdFFF] <- ""  # surrogates that stringi doesn't like

glibc <- readLines('data/glibc_widths')
utf8 <- utf8::utf8_width(cps)
stri <- stringi::stri_width(cpsa)
r3.6 <- nchar(cps, type='width')
r4.0 <- readRDS('data/r40widths.rds')
r4.0[is.na(r4.0)] <- -1L

# r4.0 <- nchar(cps, type='width', allowNA=TRUE)
# saveRDS(r4.0, 'r40widths.rds')

dat <- data.frame(
  cp=1:0x10fffd,
  r3.6, r4.0, glibc, utf8, stri
)
dat[['cp']] <- as.hexmode(dat[['cp']])
map <- match(dat[['cp']], uall[['V1']])
dat <- transform(
  dat, gc=uall[['V3']][map],
  eaw=udat[uallp[['uid']][match(cp, uallp[['id']])], 'V2'],
  desc=uall[['V2']][map]
)
ds <- subset(dat, r3.6 != r4.0)
ds <- transform(
  ds,
  gc=ifelse(is.na(gc), '??', gc),
  eaw=ifelse(is.na(eaw), '??', eaw)
)
grp <- as.integer(with(ds, interaction(r3.6, r4.0, glibc, utf8, stri, gc)))
grpi <- with(rle(grp), rep(seq_along(values), lengths))
i <- seq_along(grpi)
first <- tapply(i, grpi, '[', 1)
last <- c(first[-1] - 1, length(i))

# NOTE: the code point ranges are not all inclusive, it represents only the code
# points that are different in the code point range.

cps <- with(
  ds, paste0(
    as.character(cp[first], width=4), '..',
    as.character(cp[last], width=4)
  )
)
res <- as.data.frame(
  c(
    list(cps=cps, N=last - first + 1L), lapply(ds[2:8], '[', first),
    list(
      desc=paste0(
        strtrim(ds[['desc']][first], 12),'..',
        strtrim(ds[['desc']][last], 12)
) ) ) )

# - Analyze the errors ---------------------------------------------------------

comp_uni <- function(r4.0, glibc, utf8, stri)
  (r4.0 == glibc | glibc == - 1) &  # glibc -1 for unassigned
  (r4.0 == utf8 | utf8 > 2) &       # utf8 > 2 for unassigned and some others
  r4.0 == stri

# 1 width in R, 0 width in new:
# * 57 rows
# * 46 Marks non-spacing (Mn)
# * 2 Marks Enclosing (Me)
# * 9 Formatting controls (Cf)

width_one_to_zero <- subset(res, r3.6 != r4.0 & r3.6 == 1 & r4.0 == 0)
table(width_one_to_zero[['gc']])

# Of these 57, 36 have glib, utf8, and stri agree with the patch:

nrow(subset(width_one_to_zero, comp_uni(r4.0, glibc, utf8, stri)))

# Of the remainder, we spot checked for stri UCD10.0, and found that
# many of the Mn are just missing, i.e. Cn, so it makes sense stri returns them
# as width 1.
#
# For glibc for a9bd, that used to be 'Mc' in UCD 11.0

subset(width_one_to_zero, !comp_uni(r4.0, glibc, utf8, stri))
subset(width_one_to_zero, !(r4.0 == glibc) & glibc == 1)
subset(dat, glibc == 1 & gc == 'Cf')

# These seem okay if we ignore all the unassigned planes.  U+32FF was added in
# 12.1.0, so even `utf8` which is 12.0 is missing it.

width_one_to_greater <- subset(res, r3.6 != r4.0 & r4.0 > 1)
subset(width_one_to_greater, !comp_uni(r4.0, glibc, utf8, stri))

# Almost everyone agrees that the Mc in these should not be zero width, a bit of
# a mystery why they were considered such by R.  Some of them are Mc in the
# vicinity of Mn, but even in Unicode 8 they were not Mc

width_zero_to_greater <- subset(res, r3.6 != r4.0 & r3.6 == 0)
subset(width_zero_to_greater, !comp_uni(r4.0, glibc, utf8, stri))

# This the surrogate but addressed by 17755
# https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17755

width_greater_to_other <- subset(res, r3.6 != r4.0 & r3.6 > 1)

# Make sure we actually looked at all the differences, excluding surrogates

checked <- sort(
  unique(
    c(
      width_one_to_zero[['cps']],
      width_one_to_greater[['cps']],
      width_zero_to_greater[['cps']],
      width_greater_to_other[['cps']]
) ) )
checked <- checked[checked < 'd800..d800' | checked > 'dfff..dfff']

stopifnot(
  identical(
    sort(
      subset(
        res, r3.6 != r4.0 & cps < 'd800..d800' | cps > 'dfff..dfff'
      )[['cps']]
    ),
    checked
) )

