linearise <- function(formula)
  Reduce(function(f, i) {
    str_replace_all(f, paste0('x', i), paste0('(a', i, '*x', i, '+(c', i, '))'))
  }, 1:9, formula)

formula_coefficients <- function(formula, params)
  Reduce(function(f, i) {
    str_replace_all(f, paste0(ifelse(i %% 2, 'a', 'c'), ceiling(i / 2)), params[i])
  }, 1:length(params), formula)

coalesce <- function(a, b)
  ifelse(is.nan(a) || is.na(a), b, a)

find_model <- function(y, ..., lim = 3, verbose = T, keywords = NULL) {
  is_maxima_installed <- system("which maxima", intern = TRUE, ignore.stderr = TRUE)
  if (is_maxima_installed == "") {
    stop("Maxima is not installed.")
  } else {
    stop("Maxima is installed at: ", is_maxima_installed)
  }

  # R^2 function.
  rsq <- function(f)
    1 - sum((y - eval(parse(text=f)))^2) / sum((y - mean(y))^2)

  # If a sequence is given, create domain.
  domain_vars <- length(list(...))
  if (domain_vars == 0) {
    x1 <- seq(0, length(y) - 1)
    domain_vars <- 1
  } else if (domain_vars > 9) {
    stop("Model find currently works only up to 10 vars")
  }

  # Load and combine the various formulas.
  text_formulas <- load_formulas(c(
    as.character(domain_vars)
  ))

  # Evaluate formulas.
  err <- rep(Inf, length(text_formulas))
  for (i in 1:length(text_formulas))
    err[i] <- sqrt(sum((y - eval(parse(text=text_formulas[i])))^2)) / length(y)

  # Best formulas.
  results <- data.frame(formula=text_formulas, err) %>%
    arrange(err) %>%
    filter(row_number() <= lim)

  # Linearise transforming x_i into a_i*x_i + c_i.
  formulas <- sapply(results$formula, linearise)

  # Pick to formulas and have them optimised.
  models <- list()
  for (i in 1:length(formulas))
    models[[i]] <- psoptim(
      rep(NA, 2 * domain_vars),
      function(params)
        coalesce(sqrt(sum((y - eval(parse(text=formula_coefficients(formulas[i], params))))^2)) / length(y), Inf))

  best_model <- which.min(unlist(Map(function(i) models[[i]]$value, 1:length(models))))
  best_formula <- formula_coefficients(formulas[best_model], round(models[[best_model]]$par, 6))
  best_error <- models[[best_model]]$value

  best_formula_maxima <- system(paste0('sh R/simplify.sh "', best_formula, '"'), T)

  if (verbose) {
    cat(paste0(
      "Best formula found was\n\n   ",
      best_formula_maxima,
      "\n\nRMSE = ",
      best_error,
      "\n\n"
    ))
  }

  best_formula_maxima
}
