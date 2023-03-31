#' Create a matrix of leads and lags
#'
#' @param x a univariate signal, observed regularly
#' @param lags a vector of positive (lags) and negative (leads) numbers
#'
#' @return a matrix
#' @export
#'
#' @examples
#' x <- 1:10
#' lagmat(x, c(-2, 0, 1, 2, 3))
lagmat <- function(x, lags) {

  lags <- sort(lags)
  n <- length(x)
  k <- length(lags)
  lds <- lags[lags < 0]
  lgs <- lags[lags >= 0]
  mlg <- max(c(0, lgs))
  mld <- max(abs(c(0, lds)))

  lmat <- matrix(NA, nrow = n + mlg + mld, ncol = k)
  for (i in seq(k)) lmat[(1 + lags[i] + mld):(n + lags[i] + mld), i] <- x
  colnames(lmat) <- c(
    paste0("ahead", abs(lds)),
    paste0("lag", lgs)
  )
  lmat
}
