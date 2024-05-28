test_that("find formula", {
  x1 <- rnorm(500, mean=10, sd=2)
  x2 <- runif(500, -10, 50)

  y <- (x2/(x1 + x2*sqrt(x1)))^(-1)
  y <- sqrt(x1^2+x2^2)
  y <- sqrt(x1^2 + x2)

  # Infer formula.
  res <- find_formula(y, x1, x2)
})
