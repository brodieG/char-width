# - Assumptions ----------------------------------------------------------------

# Width in columns of East Asian Designations; will be ignored for non-default
# locales that had widths in original Rlocale_data.h

EAW <- c(N=1L, Na=1L, W=2L, F=2L, H=1L, A=1L)

# Zero Width General Categories, Cc not included by Kuhn but included by R

ZW_GC <- c('Me', 'Mn', 'Cf', 'Cc') |

# Soft hyphen is not zero width despite being Cf, as per Markus Kuhn
# Possibly should add U+0600-0605, 08E2 (Arabic Signs), U+110BD,110CD (KAITHI
# signs), (marked as Cf) and U+09BD (Mn), as gclib has those as 1 width, and
# they appear to be one (or at least some) width AFAICT.

ZW_EXCLUDE_CP <- 0x00AD

# Hangul medial vowels and terminal consonants that get merged into the base
# consonant in Korean

ZW_INCLUDE_CP <- 0x1160:0x11FF

# Unassigned wide from TR11

WIDE_UNASSIGNED <- c(
  0x3400:4DBF,     # CJK unified ideograms Extension A block
  0x4E00:9FFF,     # CJK unified ideograms block
  0xF900:FAFF,     # CJK compatibility Ideographs
  0x20000:0x2FFFF, # Supplementary Ideographic Plane
  0x30000:0x3FFFF  # Tertiary Ideographic Plane
)
# - Parse R EAW ----------------------------------------------------------------

# Parse the R data that exists, look for `table_wcwidth` and the next table
# after that, and process the data in between

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
# - Expand Points --------------------------------------------------------------

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
# - Import Unicode EAW ---------------------------------------------------------

uni_eaw <- function(file) {
  udat <- read.delim(
    stringsAsFactors = FALSE,
    file,
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
  # by marking the relevant code points as W or F (some emojis default to
  # emoji presentation, but others don't).

  with(udat,
    stopifnot(
      all(V2 %in% names(eaw)),
      all(start <= end),
      all(diff(start) > 0),
      all(diff(end) > 0)
    )
  )
  udat
}
# - Read in the Unicode Table --------------------------------------------------

uall <- read.delim(
  stringsAsFactors = FALSE,
  "UnicodeData.txt",
  comment.char = "#",
  sep = ";",
  strip.white = TRUE,
  header = FALSE
)
uall[['V1']] <- as.hexmode(uall[['V1']])

# - Zero Width Comments --------------------------------------------------------

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
 *    - Other format characters (general category code Cf in the Unicode
 *      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
 *
 *    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
 *      have a column width of 0.
 *
 *    - Added for our purposes the Unicode control characters
 *
 *    Updated based on the Unicode 12.1.0 tables at
 *    https://www.unicode.org/Public/12.1.0/ucd/UnicodeData.txt
 *    https://www.unicode.org/Public/12.1.0/ucd/EastAsianWidth.txt
 */
 "

zero_width_struct <-

"static const struct interval zero_width[] = {"


