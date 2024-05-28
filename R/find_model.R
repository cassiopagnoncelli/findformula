#' @description Transform a formula into a linearised form.
#' A formula `x1` will be transformed into `(a1*x1+(c1))`.
#' @param formula A formula in string format
#' @example linearise('x1')
linearise <- function(formula) {
  Reduce(function(form, i) {
    str_replace_all(
      form,
      paste0('x', i),
      paste0('(a', i, '*x', i, '+(c', i, '))')
    )
  }, 1:9, formula)
}

#' @description Load formulas from the `R/formulas` directory.
apply_coefficients_to_linearised_formula <- function(formul, params) {
  Reduce(function(form, i) {
    stringr::str_replace_all(
      form,
      paste0(ifelse(i %% 2 == 1, 'a', 'c'), ceiling(i / 2)),
      sprintf("%.5f", params[i])
    )
  }, seq_len(length(params)), formul)
}

#' @description Ternary if/else.
coalesce <- function(a, b)
  ifelse(is.nan(a) || is.na(a), b, a)

#' @description Load formulas from the `R/formulas` directory.
find_model <- function(y, ..., lim = 3, verbose = T, keywords = NULL) {
  is_maxima_installed <- system(
    "which maxima", intern = TRUE, ignore.stderr = TRUE)
  if (is_maxima_installed == "") {
    stop("Maxima is not installed.")
  }

  # R^2 function.
  rsq <- function(f) {
    y_hat <- eval(parse(text=f))
    1 - sum((y - y_hat)^2) / sum((y - mean(y))^2)
  }

  # If a sequence is given, create domain.
  domain_vars <- length(list(...))
  if (domain_vars == 0) {
    x1 <- seq(0, length(y) - 1)
    domain_vars <- 1
  } else if (domain_vars > 9) {
    stop("find_model currently works only up to 9 dependent variables")
  }

  # Load and combine the various formulas.
  text_formulas <- load_formulas(c(
    as.character(domain_vars)
  ))

  # Evaluate formulas.
  err <- rep(Inf, length(text_formulas))
  for (i in seq_len(length(text_formulas))) {
    y_hat <- eval(parse(text = text_formulas[i]))
    err[i] <- sqrt(sum((y - y_hat)^2)) / length(y)
  }

  # Best formulas.
  results <- data.frame(formula = text_formulas, err) %>%
    arrange(err) %>%
    filter(row_number() <= lim)

  # Linearise transforming x_i into a_i*x_i + c_i.
  formulas <- sapply(results$formula, linearise)

  # Run PSO to find the best fitting formulas
  models <- list()
  for (i in seq_len(length(formulas))) {
    models[[i]] <- psoptim(
      rep(NA, 2 * domain_vars),
      function(params) {
        form <- apply_coefficients_to_linearised_formula(formulas[i], params)
        y_hat <- eval(parse(text = form))
        coalesce(sqrt(sum((y - y_hat)^2)) / length(y), Inf)
      }
    )
  }

  best_model <- which.min(unlist(
    Map(function(i) models[[i]]$value, seq_len(length(models)))
  ))
  best_formula <- apply_coefficients_to_linearised_formula(
    formulas[best_model],
    round(models[[best_model]]$par, 6)
  )
  best_error <- models[[best_model]]$value

  if (file.exists("R/simplify.sh")) {
    best_formula_maxima <- system(
      paste0('sh R/simplify.sh "', best_formula, '"'),
      intern = TRUE
    )
  } else if (file.exists("../../R/simplify.sh")) {
    best_formula_maxima <- system(
      paste0('sh ../../R/simplify.sh "', best_formula, '"'),
      intern = TRUE
    )
  } else {
    stop("simplify.sh not found")
  }

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
