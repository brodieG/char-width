# - Unicode Dat ----------------------------------------------------------------

options(stringsAsFactors=FALSE)
uall <- read.delim(
  stringsAsFactors = FALSE,
  "UnicodeData.txt",
  comment.char = "#",
  sep = ";",
  strip.white = TRUE,
  header = FALSE
)
uall[['V1']] <- as.hexmode(uall[['V1']])

# - EAW ------------------------------------------------------------------------

source('process-lib.R')
udat <- uni_eaw("EastAsianWidth.txt")
uallp <- all_points(udat, 'u')

# - Code Points ----------------------------------------------------------------

# Generate every code point from 0x1 to 0x10fffd

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

glibc <- readLines('glibc_widths')
utf8 <- utf8::utf8_width(cps)
stri <- stringi::stri_width(cpsa)
r3.6 <- nchar(cps, type='width')
r4.0 <- readRDS('r40widths.rds')
r4.0[is.na(r4.0)] <- -1L

# r4.0 <- nchar(cps, type='width', allowNA=TRUE)
# saveRDS(r4.0, 'saveRDS(r4.0')

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

subset(width_one_to_zero, !comp_uni(r4.0, glibc, utf8, stri))

# glibc takes a stronger stance on some items that should be width 0 but it
# considered width 1. It treats Gc=='Cf' that are 'Prepended_Concatenation_Mark'
# (PCM) as width 1 (also soft-hyphen).  This is questionable for several
# reasons, though it may be appropriate in some cases.  PCMs are also known as
# subtending marks because they  extend under (sometimes over) subsequent
# characters.
#
# PCM are 'Default_Ignorable_Code_Point', which among other things mean they are
# very much implementation dependent[1], and they seem implemented unevenly at
# best.  E.g. soft-hyphen are supposed to indicate valid work-break points
# within words, and should not be shown unless a word is actually broken at that
# point.  FF does this correctly, OS X terminal just shows it, iterm2 shows a
# blank.  Note soft-hyphen is not PCM, just ignorable.  There is however some
# controversy as e.g. ISO 8859-1 states that the soft hyphen is not a
# hyphenation hint, rather, it is the hyphen displayed after a word has been
# hyphenated to break a line[11].
#
# More broadly, some of the PCMs should be treated as non-spacing or enclosing
# marks.  For example U+0605 is a supralinear mark over the subsequent numbers
# and shouldn't have advance width[2].  U+0600-0602 are less clear, with
# some typographical examples suggesting that they should advance maybe one element
# (or half) [3][9].  E.g. for the year sign Sanah, it appears it has a spot
# reserved for the calendar designation (christian/arabic) so it's zero advanced
# if it is specified, but if it isn't it advances (debatable, the actual typeset
# version in [9] shows some spacing even with the "c" specified, also, one
# source clearly shows no advance[5]).  For the footnote marker it appears to
# create an advance on the left, and the number mark an advance to the right,
# mabye.  All of these as well as U+06DD were originally proposed[5][8] as Gc ==
# 'Me'.
#
# U+0604[3] and U+110bd[7] on subtend over following characters, but look like
# more obviously like they should take up an advance element (respectively after
# and before the subtended chars).
#
# None of these to appear render correctly in browsers, OS X term, Iterm 2, with
# most of them causing varying degrees of advance and overlap.
#
# PCMs also correlate close with the prepend Grapheme Cluster Break Property[6].
#
# [1]: http://unicode.org/L2/L2002/02368-default-ignorable.html
# [2]: http://std.dkuug.dk/jtc1/sc2/wg2/docs/N3843.pdf
# [3]: https://www.unicode.org/wg2/docs/n3734.pdf
# [4]: https://www.unicode.org/L2/L2001/01428-arabic_enclosing_marks.pdf
# [5]: https://www.unicode.org/review/pri310/pri310-background.html
# [6]: https://www.unicode.org/L2/L2015/15183r-graph-cluster-brk.txt
# [7]: https://www.unicode.org/L2/L2008/08400-kaithi-num-sign.pdf
# [8]: https://www.unicode.org/wg2/docs/n2483.pdf
# [9]: https://www.unicode.org/L2/L2001/01426-arabic_marks_examples.pdf
# [11]: http://jkorpela.fi/shy.html
#
# writeLines(c('\U000110bd', '\U00011083\U000110bd', '\U000110bd\U00011083'))
# 𑂽
# 𑂃𑂽
# 𑂽𑂃
#
# writeLines('
# \u0669\u0669\u0669\u0669
# \u0600\u0669\u0669\u0669\u0669
# \u0601\u0669\u0669\u0669\u0669
# \u0602\u0669\u0669\u0669\u0669
# \u0603\u0669\u0669\u0669\u0669
# \u0604\u0669\u0669\u0669\u0669
# \u0605\u0669\u0669\u0669\u0669'
# )

subset(width_one_to_zero, !(r4.0 == glibc) & glibc == 1)
subset(dat, glibc == 1 & gc == 'Cf')

# These seem okay if we ignore all the unassigned planes.  U+32FF was not
# defined in earlier versions

width_one_to_greater <- subset(res, r3.6 != r4.0 & r4.0 > 1)
subset(width_one_to_greater, !comp_uni(r4.0, glibc, utf8, stri))

# Almost everyone agrees that these should not be zero width, a bit of a mystery
# why they were considered such by R.  Some of them are Mc in the vicinity of Mn

width_zero_to_greater <- subset(res, r3.6 != r4.0 & r3.6 == 0)
subset(width_zero_to_greater, !comp_uni(r4.0, glibc, utf8, stri))

# These include:
# * a subset of the surrogates that got there by virtue of the bug
#   in handling of them in R (where the high surrogate is used twice).
# * The I-Ching hexagrams U+4DC0..4DFF used to be considered wide, but no longer
#   are.  The glyphs seem to a little over 1 column wide on my terminal and
#   browser, though the terminal places them as being 1 wide.  glibc agrees with
#   R3.6, and this seems reasonable.  Other sources support the wide display[10]
# * Circled number ideograms e.g (10), (20), etc in U+3248-324F, which
#   definitely look wide, but terminal does not display them wide. glibc also
#   agrees with R3.6.
#
# The number ideograms are from ARIB STD 24, and it makes sense that they are
# considered wide by glibc, but what doesn't make sense is there are a lot more
# of similar 2-byte ARIB STD 24 ideographs that look like they should be
# rendered wide that are considered narrow by glibc (e.g. U+1F12F:1f169).
# Should we really single out those 8 for special treatment?
#
# Someone else also thinks this is a bug[12].  And lots of controversy in a a
# bug-report discussion.  Unicode site is down so I can't check the original
# unicode question.
#
# [10]: https://www.unicode.org/wg2/docs/n2363.pdf
# [12]: https://sourceware.org/bugzilla/show_bug.cgi?id=24658
# [13]: https://sourceware.org/bugzilla/show_bug.cgi?id=21750

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

