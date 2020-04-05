
# - Generate 0 Widths ----------------------------------------------------------

source('process-lib.r')
warning('should fill in gaps?')

# Zero width code points.

ualls <- subset(
  uall[paste0('V', c(1,2,3,11))],
  (
    # 0 width categories are enclosing, non-spacing, and
    # controls (Cc not included by Kuhn, but included by R)
    V3 %in% c('Me', 'Mn', 'Cf', 'Cc') |
    # Hangul middle or terminal components not counted
    V1 %in% 0x1160:0x11FF
  ) &
  # Soft-hyphen is considered category Cf, but has display
  # width; possibly some other should too (e.g. U+0600-0605)
  V1 != 0x00AD
)
# Groups of adjacent elements

ualls <- transform(
  ualls,
  grp=cumsum(c(FALSE, diff(V1) > 1)),
  hex=paste0("0x", toupper(as.character(V1, width=4)))
)
zlnew <- c(
  unname(
    with(
      ualls,
      tapply(hex, grp, function(x) paste0(x[c(1L, length(x))], collapse=", "))
) ) )
# - Parse Zero Widths ----------------------------------------------------------

# This is to compare to original

raw <- readLines('rlocale_data2.h')
rlstr <- grep('static const struct interval zero_width\\[\\]', raw)
txt <- raw[seq(rlstr, length(raw))]
gr <- gregexpr("0x[0-9A-F]+, 0x[0-9A-F]+", txt)
zlold <- unlist(regmatches(txt, gr))

# diffobj::diffChr(zlold, zlnew)

# - Generate Text --------------------------------------------------------------

l <- length(zlnew)
lf <- ceiling(l / 3) * 3
zlfin <- character(lf)
zlfin[seq_along(zlnew)] <- sprintf("{ %s }", zlnew)
zlfin[seq_len(length(zlnew) - 1)] <-
  paste0(zlfin[seq_len(length(zlnew) - 1)], ", ")

zlmx <- matrix(zlfin, 3)
ztxt <- do.call(paste0, c(unname(split(zlmx, row(zlmx)))))
ztxt <- sub(' $', '', ztxt)

zw_start <- grep(zero_width_start, raw, fixed=TRUE)
zw_end <- grep(zero_width_end, raw, fixed=TRUE)

stopifnot(
  length(zw_start) == 1, length(zw_end) == 1, zw_start > 2,
  zw_start < zw_end - 1
)
zfin <- c(
  raw[1:(zw_start - 1)],
  zero_width_start,
  zero_width_comment,
  zero_width_struct,
  paste0("    ", ztxt),
  "  };",
  "",
  raw[(zw_end - 1):length(raw)]
)
writeLines(zfin, 'rlocale_data2.h')

