load_formulas <- function(keywords = NULL) {
  fs <- paste('R/db', dir('R/db'), 'formulas', sep='/')

  if (!is.null(keywords)) {
    grep_res <- unique(unlist(lapply(keywords, grep, fs)))
    if (length(grep_res) > 0)
      fs <- fs[grep_res]
  }

  l <- lapply(fs, readLines)

  if (length(fs) > 1)
    unique(do.call('append', l))
  else
    l[[1]]
}
