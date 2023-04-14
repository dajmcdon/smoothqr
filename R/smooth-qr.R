#' Smoothed multi-response quantile regression
#'
#' @param x numeric matrix or data.frame of predictors. No intercept should be
#'   included.
#' @param y numeric matrix or data.frame of response values
#' @param tau the quantile(s) to be estimated. This is a number (or vector of
#'   values) strictly between 0 and 1.
#' @param degree the number of polynomials used for response smoothing. Must
#'   be no more than the number of columns in y.
#' @param intercept logical. should the model be estimated with intercept
#' @param aheads Defaults to the vector `1:ncol(y)` but if the responses are
#'   observed at a different spacing (or appear in a different order), that
#'   information should be used here.
#' @param ... additional arguments passed on to [quantreg::rq]. Not all are
#'   allowed.
#'
#' @return An object of class `smoothqr`
#' @export
#' @importFrom rlang `%||%`
#' @examples
#' x <- matrix(rnorm(100 * 10), nrow = 100)
#' y <- matrix(rnorm(100 * 10), nrow = 100)
#' out <- smooth_qr(x, y, tau = c(.25, .5, .75))
smooth_qr <- function(x, y, tau = .5, degree = 3L, intercept = TRUE,
                      aheads = 1:n_models, ...) {
  arg_is_probabilities(tau)

  arg_is_pos_int(degree)
  arg_is_lgl(intercept)
  arg_is_scalar(intercept, degree)
  n_models <- ncol(y) %||% 1L

  arg_is_numeric(aheads)
  response_names <- colnames(y) %||% paste0("y", 1:n_models)
  y <- as.matrix(y)
  arg_is_numeric(y)
  nobs <- nrow(y)

  if (n_models != length(aheads)) {
    cli::cli_abort("`length(aheads)` must be the same as the number of columns in `y`.")
  }

  if (degree == n_models) {
    cli::cli_warn(
      c("The degree of smoothing is the same as the number of responses.",
        i = "The result is computed without applying any transformations."))
    H <- diag(1, n_models, n_models)
  }
  else if (degree > n_models) {
    cli::cli_abort(
      c("The degree of smoothing must be less than the number of responses.",
        i = "Here, `degree` = {degree} with {n_models} responses."))
  } else {
    H <- matrix(1 / sqrt(length(aheads)), nrow = length(aheads), ncol = 1)
    if (degree > 1) {
      H <- cbind(H, stats::poly(aheads, degree = degree - 1, simple = TRUE))
    }
  }

  column_sds <- apply(x, 2, function(z) {
    sqrt(mean((z - mean(z, na.rm = TRUE))^2, na.rm = TRUE))
  })
  bad_cols <- which(column_sds < 1e-6)
  if (length(bad_cols) > 0L) {
    nms <- colnames(x)[bad_cols]
    if (is.null(nms)) nms <- paste("col", bad_cols)
    cli::cli_abort(
      c("Cannot perform smooth quantile regression.",
        i = "Some predictors are nearly constant. Problematic predictors:",
        i = "{nms}."))
  }

  original_predictors <- colnames(x) %||% paste0("x", 1:ncol(x))
  if (is.null(colnames(x))) colnames(x) <- original_predictors
  if (intercept) x <- cbind(Intercept = 1, x)
  colnames(H) <- paste0("degree_", 1:degree)
  x <- as.matrix(x)
  arg_is_numeric(x)
  Xtilde <- kronecker(H, as.matrix(x), make.dimnames = TRUE)
  Y <- drop(matrix(y, ncol = 1))
  dat <- data.frame(Y, Xtilde)
  rownames(dat) <- NULL
  ok <- stats::complete.cases(dat)
  dat <- dat[ok, ]
  out <- quantreg::rq(Y ~ . + 0, tau = tau, data = dat, ...)
  structure(list(
    rqfit = out,
    call = match.call(),
    H = H,
    tau = tau,
    degree = degree,
    aheads = aheads,
    intercept = intercept,
    original_predictors = original_predictors,
    response_names = response_names
  ), class = "smoothqr")
}

