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
#' @param ... additional arguments passed on to `[quantreg::rq]` not all are
#'   allowed.
#'
#' @return An object of class `smoothrq`
#' @export
#' @importFrom rlang `%||%`
#' @examples
#' x <- matrix(rnorm(100 * 10), nrow = 100)
#' y <- matrix(rnorm(100 * 10), nrow = 100)
#' out <- smooth_rq(x, y, tau = c(.25, .5, .75))
smooth_rq <- function(x, y, tau = .5, degree = 3L, intercept = TRUE,
                      aheads = 1:n_models, ...) {
  n_models <- ncol(y) %||% 1L
  response_names <- colnames(y) %||% paste0("y", 1:n_models)
  y <- as.matrix(y)
  nobs <- nrow(y)

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
    H <- stats::poly(aheads, degree = degree, simple = TRUE)
  }

  column_sds <- apply(x, 2, function(z) {
    sqrt(mean((z - mean(z, na.rm = TRUE))^2, na.rm = TRUE))
  })
  bad_cols <- which(column_sds < 1e-6)
  if (length(bad_cols) > 0L) {
    cli::cli_abort(
      c("Cannot perform smooth quantile regression.",
        i = "Some predictors are nearly constant. Problematic predictors:",
        i = "{colnames(x)[bad_cols]}."))
  }

  original_predictors <- colnames(x) %||% paste0("x", 1:ncol(x))
  if (is.null(colnames(x))) colnames(x) <- original_predictors
  if (intercept) x <- cbind(Intercept = 1, x)
  colnames(H) <- paste0("degree_", 1:degree)
  Xtilde <- kronecker(H, as.matrix(x), make.dimnames = TRUE)
  Y <- drop(matrix(y, ncol = 1))
  dat <- data.frame(Y, Xtilde)
  rownames(dat) <- NULL
  ok <- stats::complete.cases(dat)
  dat <- dat[ok, ]
  out <- quantreg::rq(Y ~ . - 1, tau = tau, data = dat, ...)
  structure(list(
    rqfit = out,
    H = H,
    tau = tau,
    degree = degree,
    aheads = aheads,
    intercept = intercept,
    original_predictors = original_predictors,
    response_names = response_names
  ), class = "smoothrq")
}


#' Extract smoothed quantile regression model coefficients
#'
#' @inheritParams stats::coef
#' @param type The option `response` means that the smoothing will be inverted,
#'   giving \eqn{\Theta H'}. This will result in a List containing one matrix for
#'   each response. Alternatively, choosing `smoothed` will return \eqn{\Theta}
#'   with the List containing one matrix for each degree.
#' @param ... not used.
#'
#' @return a List of coefficient matrices.
#' @importFrom stats coef
#' @export
#'
#' @examples
#' x <- matrix(rnorm(100 * 10), nrow = 100)
#' y <- matrix(rnorm(100 * 10), nrow = 100)
#' out <- smooth_rq(x, y, tau = c(.25, .5, .75))
#' cc1 <- coef(out)
#' cc2 <- coef(out, "smoothed")
coef.smoothrq <- function(object, type = c("response", "smoothed"), ...) {
  type <- match.arg(type)
  rlang::check_dots_empty()
  names_p <- object$original_predictors
  if (object$intercept) names_p <- c("Intercept", names_p)
  np <- length(names_p)
  nr <- length(object$response_names)
  names_d <- paste0("degree_", seq(object$degree))
  cc <- coef(object$rqfit)
  names_t <- colnames(cc)
  nt <- length(names_t)
  nd <- object$degree
  dim(cc) <- c(np, nd, nt)
  dimnames(cc) <- list(names_p, names_d, names_t)
  if (type == "response") {
    cc <- apply(cc, 3, function(x) tcrossprod(x, object$H))
    dim(cc) <- c(np, nr, nt)
    dimnames(cc) <- list(names_p, object$response_names, names_t)
    cc <- lapply(seq(nr), function(resp) cc[,resp,])
    names(cc) <- object$response_names
  } else {
    cc <- lapply(seq(nd), function(deg) cc[,deg,])
    names(cc) <- names_d
  }
  cc
}


#' Make predictions from a smoothed multi-output quantile regression
#'
#' @param object Object of class inheriting from `smooth_rq`
#' @param newdata A matrix or data frame in which to look for
#'   variables with which to predict. May NOT be omitted.
#' @param ... not used
#'
#' @return a List of predicted values, one matrix per response.
#' @importFrom stats predict
#' @export
#'
#' @examples
#' x <- matrix(rnorm(100 * 10), nrow = 100)
#' y <- matrix(rnorm(100 * 10), nrow = 100)
#' out <- smooth_rq(x, y, tau = c(.25, .5, .75))
#' p <- predict(out, newdata = x[1:10, ])
predict.smoothrq <- function(object, newdata, ...) {
  rlang::check_dots_empty()
  available_predictors <- colnames(newdata) %||% paste0("x", 1:ncol(newdata))
  if (is.null(colnames(newdata))) colnames(newdata) <- available_predictors
  H <- object$H
  resp <- object$response_names
  x <- dplyr::select(
    as.data.frame(newdata),
    dplyr::all_of(object$original_predictors)
  )
  if (object$intercept) x <- cbind(Intercept = 1, x)
  x <- as.matrix(x)
  cc <- coef(object, type = "response")
  preds_list <- lapply(cc, function(th) x %*% th)
  preds_list
}

