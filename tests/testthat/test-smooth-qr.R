test_that("tau must be a valid probability", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  expect_error(smooth_qr(x, y, tau = -0.1))
  expect_error(smooth_qr(x, y, tau = 1.1))
  expect_silent(smooth_qr(x, y, tau = c(.25, .5, .75)))
})

test_that("degree is validated", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  expect_error(smooth_qr(x, y, degree = -1L))
  expect_error(smooth_qr(x, y, degree = 1.5))
  expect_error(smooth_qr(x, y, degree = 5L))
  expect_warning(smooth_qr(x, y, degree = 4L))
})

test_that("intercept and degree must be scalar/logical", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  expect_error(smooth_qr(x, y, intercept = "yes"))
  expect_error(smooth_qr(x, y, intercept = c(TRUE, FALSE)))
  expect_error(smooth_qr(x, y, degree = c(1L, 2L)))
})

test_that("aheads must be numeric and match ncol(y)", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  expect_error(smooth_qr(x, y, aheads = letters[1:4]))
  expect_error(smooth_qr(x, y, aheads = 1:3))
  expect_silent(smooth_qr(x, y, aheads = c(1, 2, 4, 8)))
})

test_that("nearly-constant predictor columns are rejected", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  x[, 2] <- 1 # constant column

  expect_error(smooth_qr(x, y), class = "rlang_error")
})

test_that("predictors cannot already include an intercept column", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  x1 <- cbind(1, x)

  expect_error(smooth_qr(x1, y))
})

test_that("column names are inferred when missing, and kept when present", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  out <- smooth_qr(x, y)
  expect_identical(out$original_predictors, paste0("x", seq(ncol(x))))
  expect_identical(out$response_names, paste0("y", seq(ncol(y))))

  colnames(x) <- letters[seq(ncol(x))]
  colnames(y) <- LETTERS[seq(ncol(y))]
  out <- smooth_qr(x, y)
  expect_identical(out$original_predictors, letters[seq(ncol(x))])
  expect_identical(out$response_names, LETTERS[seq(ncol(y))])
})

test_that("data.frame and matrix inputs give identical results", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  colnames(x) <- letters[seq(ncol(x))]
  colnames(y) <- LETTERS[seq(ncol(y))]

  out_mat <- smooth_qr(x, y)
  out_df <- smooth_qr(as.data.frame(x), as.data.frame(y))

  expect_identical(out_mat$original_predictors, out_df$original_predictors)
  expect_identical(out_mat$response_names, out_df$response_names)
  expect_equal(coef(out_mat), coef(out_df))
})

test_that("output has the expected structure and class", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  out <- smooth_qr(x, y, tau = c(.25, .5, .75), degree = 2L)

  expect_s3_class(out, "smoothqr")
  expect_named(
    out,
    c(
      "rqfit",
      "call",
      "H",
      "tau",
      "degree",
      "aheads",
      "intercept",
      "original_predictors",
      "response_names"
    )
  )
  # multiple tau values produce class "rqs" rather than "rq"
  expect_s3_class(out$rqfit, "rqs")
  expect_equal(out$tau, c(.25, .5, .75))
  expect_equal(out$degree, 2L)
  expect_equal(dim(out$H), c(ncol(y), 2L))
})

test_that("degree = n_models reproduces separate per-response quantile regressions", {
  set.seed(9137)
  y <- matrix(rnorm(50 * 3), ncol = 3)
  x <- matrix(rnorm(50 * 3), ncol = 3)
  colnames(x) <- paste0("x", 1:3)

  expect_warning(
    out <- smooth_qr(x, y, degree = 3L, tau = 0.5)
  )
  smoothed_coefs <- coef(out, type = "response")

  separate_coefs <- lapply(1:3, function(j) {
    unname(coef(quantreg::rq(y[, j] ~ x, tau = 0.5)))
  })

  for (j in 1:3) {
    expect_equal(
      unname(smoothed_coefs[[j]]),
      separate_coefs[[j]],
      tolerance = 1e-6
    )
  }
})

test_that("rows with missing values are dropped before fitting", {
  set.seed(482)
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  x[1, 1] <- NA

  out <- smooth_qr(x, y)
  # the NA in x[1, 1] appears in every response's stacked block, so one row
  # is dropped from each of the ncol(y) blocks
  expect_equal(nrow(out$rqfit$model) / ncol(y), nrow(x) - 1)
})

test_that("intercept = TRUE and FALSE give different fitted coefficients", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out_int <- smooth_qr(x, y, tau = .5, degree = 2L, intercept = TRUE)
  out_noint <- smooth_qr(x, y, tau = .5, degree = 2L, intercept = FALSE)

  cc_int <- coef(out_int, type = "response")$y1
  cc_noint <- coef(out_noint, type = "response")$y1

  # with a single tau, coef() returns a plain named vector, not a matrix
  # slopes should differ since dropping the intercept changes the fit
  expect_false(isTRUE(all.equal(
    unname(cc_int[names(cc_int) != "Intercept"]),
    unname(cc_noint)
  )))
})
