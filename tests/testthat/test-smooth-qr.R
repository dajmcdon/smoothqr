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

test_that("coef() and predict() work on the fitted object", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  out <- smooth_qr(x, y)
  expect_silent(cc <- coef(out))
  expect_silent(preds <- predict(out, newdata = x[1:3, ]))

  expect_length(cc, ncol(y))
  expect_length(preds, ncol(y))
  expect_equal(nrow(preds[[1]]), 3L)
})

test_that("coef() has the documented dimensions, names and types", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L)

  cc_smoothed <- coef(out, type = "smoothed")
  expect_type(cc_smoothed, "list")
  expect_named(cc_smoothed, paste0("degree_", 1:2))
  for (deg in cc_smoothed) {
    # rows: intercept + predictors; columns: one per tau
    expect_equal(dim(deg), c(6, 2))
    expect_identical(
      rownames(deg),
      c("Intercept", colnames(x))
    )
    expect_identical(colnames(deg), c("tau = 0.25", "tau = 0.5"))
  }

  cc_response <- coef(out, type = "response")
  expect_type(cc_response, "list")
  expect_named(cc_response, colnames(y))
  for (resp in cc_response) {
    expect_equal(dim(resp), c(6, 2))
    expect_identical(rownames(resp), c("Intercept", colnames(x)))
    expect_identical(colnames(resp), c("tau = 0.25", "tau = 0.5"))
  }
})

test_that("coef(type = 'response') correctly inverts the smoothing transform", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L)
  cc_smoothed <- coef(out, type = "smoothed")
  cc_response <- coef(out, type = "response")

  # Response-scale coefficients should equal H %*% (stacked smoothed
  # coefficients), applied separately for each predictor/tau combination.
  for (j in seq_len(ncol(y))) {
    for (k in seq_along(out$tau)) {
      manual <- sapply(cc_smoothed, function(deg) deg[, k]) %*% out$H[j, ]
      expect_equal(
        unname(cc_response[[j]][, k]),
        unname(drop(manual)),
        tolerance = 1e-8
      )
    }
  }
})

test_that("coef() type = 'response' reproduces separate quantile regressions when H is identity", {
  set.seed(9137)
  y <- matrix(rnorm(50 * 3), ncol = 3)
  x <- matrix(rnorm(50 * 3), ncol = 3)
  colnames(x) <- paste0("x", 1:3)

  expect_warning(out <- smooth_qr(x, y, degree = 3L, tau = c(.3, .7)))
  cc_response <- coef(out, type = "response")

  for (j in 1:3) {
    for (tau in c(.3, .7)) {
      manual <- unname(coef(quantreg::rq(y[, j] ~ x, tau = tau)))
      col <- paste("tau =", tau)
      expect_equal(unname(cc_response[[j]][, col]), manual, tolerance = 1e-6)
    }
  }
})

test_that("predict() numerically matches manual matrix multiplication of coef(type = 'response')", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L)
  newdata <- x[1:10, ]
  preds <- predict(out, newdata = newdata)
  cc <- coef(out, type = "response")

  for (resp in names(cc)) {
    manual <- cbind(Intercept = 1, newdata) %*% cc[[resp]]
    expect_equal(unname(preds[[resp]]), unname(manual))
  }
})

test_that("predict() output has expected names and dimensions", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.1, .5, .9))
  newdata <- x[1:7, ]
  preds <- predict(out, newdata = newdata)

  expect_named(preds, colnames(y))
  for (p in preds) {
    expect_equal(dim(p), c(7, 3))
    expect_identical(colnames(p), paste("tau =", c(.1, .5, .9)))
  }
})

test_that("predict() ignores extra columns and column order in newdata", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y)
  newdata_extra <- cbind(x[1:5, ], extra = rnorm(5))
  newdata_reordered <- x[1:5, rev(seq_len(ncol(x)))]

  preds_orig <- predict(out, newdata = x[1:5, ])
  preds_extra <- predict(out, newdata = newdata_extra)
  preds_reordered <- predict(out, newdata = newdata_reordered)

  expect_equal(preds_extra, preds_orig)
  expect_equal(preds_reordered, preds_orig)
})

test_that("predict() errors informatively when newdata is missing required predictors", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y)
  newdata_missing <- x[1:5, 1:3]

  expect_error(predict(out, newdata = newdata_missing))
})

test_that("predict() propagates NA only for affected rows, leaving other rows unaffected", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L)

  newdata_complete <- x[1:5, ]
  newdata_na <- newdata_complete
  newdata_na[2, 3] <- NA

  # smooth_qr's own current behavior: a single NA in row 2 doesn't error,
  # rather it produces NA predictions for row 2 only, via ordinary matrix
  # multiplication (no expect_error here, since no validation currently
  # occurs in predict.smoothqr for missing values)
  preds_na <- predict(out, newdata = newdata_na)
  preds_complete <- predict(out, newdata = newdata_complete)

  for (resp in names(preds_na)) {
    expect_true(all(is.na(preds_na[[resp]][2, ])))
    expect_false(anyNA(preds_na[[resp]][-2, ]))
    expect_equal(preds_na[[resp]][-2, ], preds_complete[[resp]][-2, ])
  }
})

test_that("coef() drops the intercept row when intercept = FALSE", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L, intercept = FALSE)

  cc_smoothed <- coef(out, type = "smoothed")
  for (deg in cc_smoothed) {
    expect_equal(dim(deg), c(5, 2))
    expect_identical(rownames(deg), colnames(x))
  }

  cc_response <- coef(out, type = "response")
  for (resp in cc_response) {
    expect_equal(dim(resp), c(5, 2))
    expect_identical(rownames(resp), colnames(x))
  }
})

test_that("coef(type = 'response') without intercept reproduces separate no-intercept quantile regressions", {
  set.seed(9137)
  y <- matrix(rnorm(50 * 3), ncol = 3)
  x <- matrix(rnorm(50 * 3), ncol = 3)
  colnames(x) <- paste0("x", 1:3)

  expect_warning(
    out <- smooth_qr(x, y, degree = 3L, tau = 0.5, intercept = FALSE)
  )
  cc_response <- coef(out, type = "response")

  for (j in 1:3) {
    manual <- unname(coef(quantreg::rq(y[, j] ~ x + 0, tau = 0.5)))
    # with a single tau, coef() returns a plain named vector, not a matrix
    expect_equal(unname(cc_response[[j]]), manual, tolerance = 1e-6)
  }
})

test_that("predict() without intercept omits the intercept column and matches manual multiplication", {
  set.seed(5824)
  y <- matrix(rnorm(50 * 4), ncol = 4)
  x <- matrix(rnorm(50 * 5), ncol = 5)
  colnames(x) <- paste0("x", 1:5)
  colnames(y) <- paste0("y", 1:4)

  out <- smooth_qr(x, y, tau = c(.25, .5), degree = 2L, intercept = FALSE)
  newdata <- x[1:10, ]
  preds <- predict(out, newdata = newdata)
  cc <- coef(out, type = "response")

  for (resp in names(cc)) {
    manual <- newdata %*% cc[[resp]]
    expect_equal(unname(preds[[resp]]), unname(manual))
  }
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
