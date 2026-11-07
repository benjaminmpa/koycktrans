test_that("plusieurs variables explicatives", {
  set.seed(1)
  n <- 500
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- numeric(n)
  for (t in 2:n) y[t] <- 0.5*y[t-1] + 0.4*x1[t] + 0.3*x2[t] + rnorm(1, sd = 0.3)
  mod <- koyck_model(y ~ x1 + x2, data = data.frame(y = y, x1 = x1, x2 = x2))
  expect_length(mod$beta_0, 2)
  expect_equal(mod$lambda, 0.5, tolerance = 0.1)
})
test_that("koyck_model retrouve les parametres", {
  set.seed(123)
  n <- 2000
  x <- rnorm(n)
  y <- numeric(n)
  for (t in 2:n) y[t] <- 2*(1 - 0.6) + 0.6*y[t-1] + 0.5*x[t] + rnorm(1, sd = 0.3)
  mod <- koyck_model(y ~ x, data = data.frame(y = y, x = x))

  expect_s3_class(mod, "koyck")
  expect_equal(mod$lambda, 0.6, tolerance = 0.1)
  expect_equal(unname(mod$beta_0), 0.5, tolerance = 0.1)
  expect_equal(mod$alpha, 2, tolerance = 0.15)
  expect_equal(mod$mean_lag, mod$lambda / (1 - mod$lambda))
})

test_that("erreur si aucune variable explicative", {
  d <- data.frame(y = rnorm(10))
  expect_error(koyck_model(y ~ 1, data = d))
})
