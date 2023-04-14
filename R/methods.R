#' Extract smoothed quantile regression model coefficients
#'
#' @inheritParams stats::coef
#' @param type The option `"response"` means that the smoothing will be inverted,
#'   giving \eqn{\Theta H'}. This will result in a List containing one matrix for
#'   each response. Alternatively, choosing `"smoothed"` will return \eqn{\Theta}
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
#' out <- smooth_qr(x, y, tau = c(.25, .5, .75))
#' cc1 <- coef(out)
#' cc2 <- coef(out, "smoothed")
coef.smoothqr <- function(object, type = c("response", "smoothed"), ...) {
  type <- match.arg(type)
  rlang::check_dots_empty()
  names_p <- object$original_predictors
  if (object$intercept) names_p <- c("Intercept", names_p)
  np <- length(names_p)
  nr <- length(object$response_names)
  names_d <- paste0("degree_", seq(object$degree))
  cc <- coef(object$rqfit)
  names_t <- paste0("tau = ", object$tau)
  nt <- length(names_t)
  nd <- object$degree
  dim(cc) <- c(np, nd, nt)
  dimnames(cc) <- list(names_p, names_d, names_t)
  if (type == "response") {
    cc <- apply(cc, 3, function(x) tcrossprod(x, object$H))
    dim(cc) <- c(np, nr, nt)
    dimnames(cc) <- list(names_p, object$response_names, names_t)
    cc <- lapply(seq(nr), function(resp) cc[ , resp, ])
    names(cc) <- object$response_names
  } else {
    cc <- lapply(seq(nd), function(deg) cc[ , deg, ])
    names(cc) <- names_d
  }
  cc
}


#' Make predictions from a smoothed multi-output quantile regression
#'
#' @param object Object of class inheriting from `smooth_qr`
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
#' out <- smooth_qr(x, y, tau = c(.25, .5, .75))
#' p <- predict(out, newdata = x[1:10, ])
predict.smoothqr <- function(object, newdata, ...) {
  rlang::check_dots_empty()
  available_predictors <- colnames(newdata) %||% paste0("x", 1:ncol(newdata))
  if (is.null(colnames(newdata))) colnames(newdata) <- available_predictors
  newdata <- newdata[, available_predictors, drop = FALSE]
  predictor_set <- object$original_predictors %in% available_predictors
  if (!all(predictor_set)) {
    missing_predictors <- object$original_predictors[!predictor_set]
    cli::cli_abort(
      c("Some of the original predictors are not present in `newdata`.",
        i = "Missing {missing_predictors}."))
  }
  if (object$intercept) newdata <- cbind(Intercept = 1, newdata)
  newdata <- as.matrix(newdata)
  cc <- coef(object, type = "response")
  preds_list <- lapply(cc, function(th) {
    p <- newdata %*% th
    colnames(p) <- paste("tau =", object$tau)
    p
  })
  preds_list
}

#' @method summary smoothqr
#' @export
summary.smoothqr <- function(object, ...) {
  rlang::check_dots_empty()

  out <- structure(
    list(
      call = object$call,
      degree = object$degree,
      aheads = object$ahead,
      tau = object$tau,
      coefs = dplyr::bind_rows(
        lapply(coef(object), tibble::as_tibble),
        .id = "response"
      )
    ),
    class = "summary.smoothqr")
  out
}

#' @method print summary.smoothqr
#' @export
print.summary.smoothqr <- function(x, ...) {
  rlang::check_dots_empty()
  cat("\nCall: ", deparse(x$call), "\n", fill = TRUE)
  cat("Degree: ", x$degree, "\n")
  cat("tau: ", x$tau, "\n", fill = TRUE)
  cat("aheads: ", x$aheads, "\n", fill = TRUE)
  cat("Coefficients:\n")
  print(x$coefs)
  cat("\n")
}

#' @method print smoothqr
#' @export
print.smoothqr <- function(x, ...) {
  print(summary(x, ...))
}

