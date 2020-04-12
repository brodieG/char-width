# - Utils ----------------------------------------------------------------------

# Convert standard unicode code point ranges to start/end columns
# Assumes code points are in "V1" column

range_to_points <- function(dat) {
  stopifnot(!c('start', 'end') %in% names(dat))
  points <- strsplit(dat[['V1']], '..', fixed=TRUE)
  lens <- lengths(points)
  stopifnot(all(lens > 0 & lens < 3))
  for(i in seq_along(points)) length(points[[i]]) <- 2
  points <- matrix(unlist(points), 2)

  dat[['start']] <- points[1,]
  dat[['end']] <- points[2,]
  dat <- within(dat, end[is.na(end)] <- start[is.na(end)])
  dat[c('start', 'end')] <- lapply(dat[c('start', 'end')], as.hexmode)

  # Sanity checks

  with(dat,
    stopifnot(
      all(start <= end),
      all(diff(start) > 0),
      all(diff(end) > 0)
  ) )
  dat
}
# Expand code point ranges to full point range

all_points <- function(dat, prefix) {
  sizes <- with(dat, end - start + 1L)
  allp <- with(
    dat,
    data.frame(
      id=rep(start - 1L, sizes) + sequence(sizes),
      id2=rep(seq_len(nrow(dat)), sizes)
  ) )
  setNames(allp, c('id', paste0(prefix, 'id')))
}
# - Assumptions ----------------------------------------------------------------

# Width in columns of East Asian Designations; will be ignored for non-default
# locales that had widths in original Rlocale_data.h

EAW <- c(N=1L, Na=1L, W=2L, F=2L, H=1L, A=1L)

# Zero Width General Categories, Cc not included by Kuhn but included by R

ZW_GC <- c('Me', 'Mn', 'Cf', 'Cc')

# Zero Width Exclusions

prop_list <- range_to_points(
  subset(
    read.delim(   # borrowed from Gábor
      stringsAsFactors = FALSE,
      "data/propList.txt",
      comment.char = "#",
      sep = ";",
      strip.white = TRUE,
      header = FALSE
    ),
    V2 == 'Prepended_Concatenation_Mark'
) )
ZW_EXCLUDE_CP <- c(
  0x00AD,                                                # soft hyphen
  all_points(prop_list[c('start', 'end')], 'pcm')[[1]]   # prepending marks
)
# Hangul medial vowels and terminal consonants that get merged into the base
# consonant in Korean

ZW_INCLUDE_CP <- 0x1160:0x11FF

# Unassigned wide from TR11: oddly these are somewhat indirectly included in the
# EAW data though missing e.g 2FFFF and 3FFFF.
#
# CURRENTLY UNUSED as already in EAW except for the two points above.

WIDE_UNASSIGNED <- c(
   0x3400:0x4DBF,   # CJK unified ideograms Extension A block
   0x4E00:0x9FFF,   # CJK unified ideograms block
   0xF900:0xFAFF,   # CJK compatibility Ideographs
  0x20000:0x2FFFF,  # Supplementary Ideographic Plane
  0x30000:0x3FFFF   # Tertiary Ideographic Plane
)
# Additional wide characters for Glibc

WIDE_GLIBC <- c(
  0x3248:0x324F,     # ARIB STD 24 speed limits
  0x4DC0:0x4DFF      # I Ching hexagrams
)

# - Parse R EAW ----------------------------------------------------------------

# Parse the R EAW data that exists in rlocale_data.h, look for `table_wcwidth`
# and the next table after that, and process the data in between

options(stringsAsFactors=FALSE)

parse_rlocale <- function(rlocale) {
  raw <- readLines(rlocale)
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
  # unicode point inside an ifdef block.  If some of these assumptions
  # change a lot of code will need to change.

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
  list(
    lbase=lbase, lwin=lwin, txt1=txt1, isifdef=isifdef,
    lwin=lwin, txt2=txt2, matches=matches, rdat=rdat,
    rlstr=rlstr, rlend=rlend, raw=raw
  )
}
# - Import Unicode EAW ---------------------------------------------------------

uni_eaw <- function(file) {
  udat <- read.delim(   # borrowed from Gábor
    stringsAsFactors = FALSE,
    file,
    comment.char = "#",
    sep = ";",
    strip.white = TRUE,
    header = FALSE
  )
  # Generate and validate the ranges

  udat <- range_to_points(udat)

  # we rely on EastAsianWidth correctly representing emoji presentation
  # by marking the relevant code points as W or F (some emojis default to
  # emoji presentation, but others don't).

  # Sanity checks

  with(udat, stopifnot(all(V2 %in% names(EAW))))
  udat
}
# - Read in the Unicode Table --------------------------------------------------

uall <- read.delim(
  stringsAsFactors = FALSE,
  "data/UnicodeData.txt",
  comment.char = "#",
  sep = ";",
  strip.white = TRUE,
  header = FALSE
)
uall[['V1']] <- as.hexmode(uall[['V1']])

# - Wide Comments --------------------------------------------------------------

wide_comment <- "
/* glibc treats the ARIB STD 24 speed limit numbers U+3248-324F and the I Ching
 * hexagrams U+4DC0-4DFF as wide, so we do too.
 */
"
# - Zero Width Comments --------------------------------------------------------

# Comments that we'll need to update in the final file

zero_width_start <-

"/* -------------------helper for wcwidth -------------------- */"

zero_width_end <-

"zero_width_count = (sizeof(zero_width)/sizeof(struct interval));"

zero_width_comment <-

"
/* From http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 *    - Non-spacing and enclosing combining characters (general
 *      category code Mn or Me in the Unicode database) have a
 *      column width of 0.
 *
 *    - SOFT HYPHEN (U+00AD) has a column width of 1.
 *
 *    - Prepended_Concatenation_Mark have column width of 1.
 *
 *    - Other format characters (general category code Cf in the Unicode
 *      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
 *
 *    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
 *      have a column width of 0.
 *
 *    - C0 and C1 control characters have a column width of 0.
 *
 *    Updated based on the Unicode 12.1.0 tables at
 *    https://www.unicode.org/Public/12.1.0/ucd/UnicodeData.txt
 *    https://www.unicode.org/Public/12.1.0/ucd/EastAsianWidth.txt
 *
 *    And reflecting practices in glibc's wcwidth()
 */
 "

zero_width_struct <-

"static const struct interval zero_width[] = {"


