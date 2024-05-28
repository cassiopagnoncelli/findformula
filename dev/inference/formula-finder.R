library('dplyr')

options(warn=-1)

load_formulas <- function(keywords = NULL) {
  fs <- paste('db', dir('db'), 'formulas', sep='/')
  
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

formula_find <- function(y, ..., verbose=T, keywords=NULL) {
  # R^2 function.
  rsq <- function(f)
    1 - sum((y - eval(parse(text=f)))^2) / sum((y - mean(y))^2)
  
  # If a sequence is given, create domain.
  domain_vars <- length(list(...))
  if (domain_vars == 0) {
    x1 <- seq(0, length(y) - 1)
    domain_vars <- 1
  }
  
  # Load and combine the various formulas.
  text_formulas <- load_formulas(c(
    as.character(domain_vars)
  ))
  
  # Evaluate formulas.
  err <- rep(Inf, length(text_formulas))
  for (i in 1:length(text_formulas))
    err[i] <- sqrt(sum((y - eval(parse(text=text_formulas[i])))^2)) / length(y)
  
  # Best formula.
  results <- data.frame(formula=text_formulas, err) %>% 
    arrange(err) %>% 
    filter(row_number() == 1)
  
  if (verbose) {
    if (abs(results$err) < 1e-6)
      cat(paste0('Found this one (exact): ', results$formula, '\n'))
    else
      cat(paste0('Found this one (approx): ', results$formula, '\n',
                 '  RMSE=', results$err, '   R^2=', rsq(results$formula), '\n'))
  }
  
  as.character(results$formula)
}
