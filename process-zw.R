
# - Generate 0 Widths ----------------------------------------------------------

source('process-lib.R')

# Zero width code points.

ualls <- subset(
  uall[paste0('V', c(1,2,3,11))],
  (
    # 0 width categories
    V3 %in% ZW_GC |
    # extra 0 width CPs
    V1 %in% ZW_INCLUDE_CP
  ) &
  # CPs that are not zero-width despite General Category
  (!V1 %in% ZW_EXCLUDE_CP)
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
#
# - Generate Text --------------------------------------------------------------

raw <- readLines('rlocale_data2.h')
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

