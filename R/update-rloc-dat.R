# PROCESS ASSUMES WORKING DIRECTORY IS SET TO REPO ROOT
#
# - Apply initial patch to original file ---------------------------------------

# Original file had some overlapping ranges, etc.

if(
  inherits(
    file.copy(
      'src/rlocale_data-r78347.h', 'src/rlocale_data.h', overwrite=TRUE
    ),
    "try-error"
  )
)
  stop("Failed restoring rlocale file")

system2('patch', c('src/rlocale_data.h', 'patches/patch-rloc-dat-init.txt'))

# - Process Locale File --------------------------------------------------------

source("R/process-lib.R")

# Process EastAsianWidth.txt and modify `table_wcwidth[]` in rlocale_data.h

source("R/process-ea.R")

# Update zero_width[] rlocale_data

source("R/process-zw.R")

# output is src/rlocale-data.h
# git diff --no-index src/rlocale-data*
