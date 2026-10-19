# helpers ---------------------------------------------------------------
design <- function(x, z) unname(cbind(1, x, z, x * z))

# QX / QD ---------------------------------------------------------------
test_that("QX has correct structure and known entries", {
  M <- QX(mean_x = 5, mean_z = 3, var_x = 1, var_z = 2, cov_xz = 0.2)

  expect_equal(dim(M), c(4, 4))
  expect_true(isSymmetric(M))
  expect_false(anyNA(M))
  expect_equal(M[1, 1], 1)
  expect_equal(M[1, 2], 5)                  # E[x]
  expect_equal(M[1, 3], 3)                  # E[z]
  expect_equal(M[2, 2], 1 + 5^2)            # E[x^2]
  expect_equal(M[3, 3], 2 + 3^2)            # E[z^2]
  expect_equal(M[2, 3], 0.2 + 5 * 3)        # E[xz]
  expect_equal(M[1, 4], M[2, 3])
})

test_that("QX matches a Monte Carlo moment matrix (normal predictors)", {
  set.seed(123)
  n  <- 5e5
  mu <- c(1, 0.5)
  S  <- matrix(c(1, 0.3, 0.3, 1.5), 2)
  xz <- mvtnorm::rmvnorm(n, mu, S)
  X  <- design(xz[, 1], xz[, 2])

  expect_equal(
    QX(mu[1], mu[2], S[1, 1], S[2, 2], S[1, 2]),
    crossprod(X) / n,
    tolerance = 0.02
  )
})

test_that("QX + QD matches the moment matrix of error-contaminated predictors", {
  set.seed(456)
  n  <- 5e5
  mu <- c(1, 0.5)
  S  <- matrix(c(1, 0.3, 0.3, 1.5), 2)
  vex <- 0.25
  vez <- 0.4

  xz <- mvtnorm::rmvnorm(n, mu, S)
  xs <- xz[, 1] + rnorm(n, 0, sqrt(vex))
  zs <- xz[, 2] + rnorm(n, 0, sqrt(vez))
  Xs <- design(xs, zs)

  QXm <- QX(mu[1], mu[2], S[1, 1], S[2, 2], S[1, 2])
  QDm <- QD(vex, vez, S[1, 1], S[2, 2], mu[1], mu[2])

  expect_equal(QXm + QDm, crossprod(Xs) / n, tolerance = 0.02)
})

test_that("QD is zero when there is no measurement error", {
  expect_equal(QD(0, 0, 1, 1, 5, 3), matrix(0, 4, 4))
})

# E_Xy ------------------------------------------------------------------
test_that("E_Xy picks out columns of the moment matrix", {
  M <- QX(2, 1, 1, 1, 0.1)
  expect_equal(as.numeric(E_Xy(c(1, 0, 0, 0), M)), M[, 1])
  expect_equal(as.numeric(E_Xy(c(0, 0, 0, 1), M)), M[, 4])
})

# error variance of y / residual measures -------------------------------
test_that("without measurement error the full model reproduces R2_true", {
  M    <- QX(5, 3, 1, 1, 0)
  beta <- c(0, 1, 1, -0.75)
  E_y  <- error_variance_y(beta, M, R2_true = 0.8)

  res <- residual_measures(beta, M, XE = M, EXY = E_Xy(beta, M),
                           E_y = E_y, interaction = TRUE)

  expect_equal(res[1], E_y)                 # residual = pure noise
  expect_equal(res[2], 0.8)                 # R2
  expect_equal(res[3], sqrt(E_y))           # RMSE
})

test_that("reduced model is never better than the full model (no error)", {
  M    <- QX(5, 3, 1, 1, 0.2)
  beta <- c(0, 1, 1, -0.75)
  E_y  <- error_variance_y(beta, M, 0.8)
  Xy   <- E_Xy(beta, M)

  full <- residual_measures(beta, M, M, Xy, E_y, interaction = TRUE)
  red  <- residual_measures(beta, M, M, Xy, E_y, interaction = FALSE)

  expect_gte(red[1], full[1] - 1e-10)
  expect_lte(red[2], full[2] + 1e-10)
})

test_that("full and reduced model coincide when the true interaction is zero", {
  M    <- QX(5, 3, 1, 1, 0.2)
  beta <- c(0, 1, 1, 0)
  E_y  <- error_variance_y(beta, M, 0.5)
  Xy   <- E_Xy(beta, M)

  full <- residual_measures(beta, M, M, Xy, E_y, interaction = TRUE)
  red  <- residual_measures(beta, M, M, Xy, E_y, interaction = FALSE)

  expect_equal(full, red)
})

test_that("measurement error increases the expected residual", {
  M    <- QX(5, 3, 1, 1, 0)
  beta <- c(0, 1, 1, -0.75)
  E_y  <- error_variance_y(beta, M, 0.8)
  Xy   <- E_Xy(beta, M)
  Qst  <- M + QD(0.25, 0.25, 1, 1, 5, 3)

  no_err <- residual_measures(beta, M, M,   Xy, E_y, TRUE)
  err    <- residual_measures(beta, M, Qst, Xy, E_y, TRUE)

  expect_gt(err[1], no_err[1])
  expect_lt(err[2], no_err[2])
})

test_that("error_variance_y is monotone in R2_true", {
  M    <- QX(5, 3, 1, 1, 0)
  beta <- c(0, 1, 1, -0.75)
  expect_gt(error_variance_y(beta, M, 0.5), error_variance_y(beta, M, 0.9))
})

# AIC / BIC -------------------------------------------------------------
test_that("aic() and bic() agree with stats::AIC / BIC for lm", {
  set.seed(1)
  n <- 100
  x <- rnorm(n); z <- rnorm(n)
  y <- 1 + x + z + rnorm(n)
  fit <- lm(y ~ x + z)
  ete <- sum(residuals(fit)^2)
  k   <- length(coef(fit)) + 1             # coefficients + sigma

  expect_equal(aic(ete, n, k), AIC(fit))
  expect_equal(bic(ete, n, k), BIC(fit))
})

# surfaces and tangents -------------------------------------------------
test_that("surfaces have the right dimension and values", {
  b  <- c(1, 2, 3, 4)
  xs <- seq(-1, 1, length.out = 5)
  zs <- seq(-2, 2, length.out = 7)

  cs <- curved_surface(b, xs, zs)
  ps <- plane_surface(b, xs, zs)

  expect_equal(dim(cs), c(length(zs), length(xs)))   # rows = z, cols = x
  expect_equal(dim(ps), c(length(zs), length(xs)))
  expect_equal(cs[zs == 0, xs == 0], b[1])           # value at the origin
  expect_equal(cs[7, 5], f_tangent(b, x = 1, z = 2)) # corner
  expect_equal(ps[7, 5], b[1] + b[2] * 1 + b[3] * 2) # no interaction term
})

test_that("tangent lines touch the surface at the evaluation point", {
  b  <- c(1, 2, 3, 4)
  ev <- make_ev_point(b, x = 0.5, z = -1)

  expect_equal(ev$y, f_tangent(b, 0.5, -1))
  expect_equal(make_tangent_x(b, ev, z_seq = ev$z), ev$y)
  expect_equal(make_tangent_z(b, ev, x_seq = ev$x), ev$y)
  # slope along z at x = 0.5 is b3 + b4 * x
  expect_equal(diff(make_tangent_x(b, ev, c(0, 1))), b[3] + b[4] * 0.5)
})

# skew helpers ----------------------------------------------------------
test_that("get_dp_safe signals the normal path when skewness is zero", {
  expect_null(get_dp_safe(c(0, 0), 1, 1, 0, c(0, 0)))
})

test_that("rmvsn_safe returns an n x 2 matrix with roughly correct moments", {
  set.seed(1)
  xz <- rmvsn_safe(2e4, dp = NULL, mean = c(2, 3), var1 = 1, var2 = 2, cov = 0.5)
  expect_equal(dim(xz), c(2e4, 2))
  expect_equal(colMeans(xz), c(2, 3), tolerance = 0.05)
  expect_equal(unname(cov(xz)[1, 2]), 0.5, tolerance = 0.1)
})

test_that("rmvsn_safe works for skew-normal predictors", {
  skip_if_not_installed("sn")
  set.seed(1)
  dp <- get_dp_safe(c(2, 3), 1, 2, 0.3, c(0.3, 0.3))
  xz <- rmvsn_safe(1e4, dp, c(2, 3), 1, 2, 0.3)
  expect_equal(colMeans(xz), c(2, 3), tolerance = 0.1)
})
