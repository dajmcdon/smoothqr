test_that("lagmat works with lags only (previously broken)", {
  x <- 1:10
  out <- lagmat(x, c(0, 1, 2, 3))

  expect_equal(dim(out), c(13L, 4L))
  expect_identical(colnames(out), c("lag0", "lag1", "lag2", "lag3"))
  expect_equal(out[, "lag0"], c(x, NA, NA, NA))
  expect_equal(out[, "lag1"], c(NA, x, NA, NA))
})

test_that("lagmat works with leads only (previously broken)", {
  x <- 1:10
  out <- lagmat(x, c(-1, -2, -3))

  expect_equal(dim(out), c(13L, 3L))
  expect_identical(colnames(out), c("ahead3", "ahead2", "ahead1"))
  expect_equal(out[, "ahead1"], c(NA, NA, x, NA))
  expect_equal(out[, "ahead3"], c(x, NA, NA, NA))
})

test_that("lagmat works with a single lag or a single lead", {
  x <- 1:10

  out_lag <- lagmat(x, 2)
  expect_equal(dim(out_lag), c(12L, 1L))
  expect_identical(colnames(out_lag), "lag2")

  out_lead <- lagmat(x, -2)
  expect_equal(dim(out_lead), c(12L, 1L))
  expect_identical(colnames(out_lead), "ahead2")
})

test_that("lagmat works with a mix of lags and leads", {
  x <- 1:10
  out <- lagmat(x, c(-2, 0, 1, 2, 3))

  expect_equal(dim(out), c(15L, 5L))
  expect_identical(colnames(out), c("ahead2", "lag0", "lag1", "lag2", "lag3"))
  expect_equal(out[, "lag0"], c(NA, NA, x, NA, NA, NA))
  expect_equal(out[, "ahead2"], c(x, NA, NA, NA, NA))
})

test_that("lagmat includes lag0 correctly when 0 is among the lags", {
  x <- 1:5
  out <- lagmat(x, c(0, 1))

  expect_equal(out[, "lag0"], c(x, NA))
  expect_equal(out[, "lag1"], c(NA, x))
})
