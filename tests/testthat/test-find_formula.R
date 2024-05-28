test_that("find formula: 2 vars with sqrt and division", {
  x1 <<- rnorm(500, mean = 10, sd = 2)
  x2 <<- runif(500, -10, 50)
  y <<- (x2/(x1 + x2*sqrt(x1)))^(-1)
  res <- find_formula(y, x1, x2)
  expect_gt(length(res), 0)
})

test_that("find formula: polynomial", {
  x1 <<- rnorm(500, mean = 10, sd = 2)
  x2 <<- runif(500, -10, 50)
  y <<- sqrt(x1^2+x2^2)
  res <- find_formula(y, x1, x2)
  expect_gt(length(res), 0)
})

test_that("find formula: quadratic", {
  x1 <<- rnorm(500, mean = 10, sd = 2)
  x2 <<- runif(500, -10, 50)
  y <<- sqrt(x1^2 + x2)
  res <- find_formula(y, x1, x2)
  expect_gt(length(res), 0)
})
