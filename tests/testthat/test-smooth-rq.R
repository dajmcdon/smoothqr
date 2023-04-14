test_that("smoothqr handles the basics", {
  y <- matrix(rnorm(100), ncol = 4)
  x <- matrix(rnorm(125), ncol = 5)

  expect_warning(smooth_qr(x, y, degree = 4L))
  expect_error(smooth_qr(x, y, degree = 5L))

  expect_silent(smooth_qr(x, y))

  out <- smooth_qr(x, y)
  expect_identical(out$original_predictors, paste0("x", seq(ncol(x))))
  expect_identical(out$response_names, paste0("y", seq(ncol(y))))

  x1 <- cbind(1, x)
  expect_error(smooth_qr(x1, y))

  colnames(x) <-  letters[seq(ncol(x))]
  colnames(y) <- LETTERS[seq(ncol(y))]
  out <- smooth_qr(x, y)
  expect_identical(out$original_predictors, letters[seq(ncol(x))])
  expect_identical(out$response_names, LETTERS[seq(ncol(y))])

  y <- as.data.frame(y)
  x <- as.data.frame(x)
  out <- smooth_qr(x, y)
  expect_identical(out$original_predictors, letters[seq(ncol(x))])
  expect_identical(out$response_names, LETTERS[seq(ncol(y))])

  expect_silent(coef(out))
  expect_silent(predict(out, newdata = x[1:3, ]))
})
