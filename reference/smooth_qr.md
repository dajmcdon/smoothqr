# Smoothed multi-response quantile regression

Smoothed multi-response quantile regression

## Usage

``` r
smooth_qr(
  x,
  y,
  tau = 0.5,
  degree = 3L,
  intercept = TRUE,
  aheads = 1:n_models,
  ...
)
```

## Arguments

- x:

  numeric matrix or data.frame of predictors. No intercept should be
  included.

- y:

  numeric matrix or data.frame of response values

- tau:

  the quantile(s) to be estimated. This is a number (or vector of
  values) strictly between 0 and 1.

- degree:

  the number of polynomials used for response smoothing. Must be no more
  than the number of columns in y.

- intercept:

  logical. should the model be estimated with intercept

- aheads:

  Defaults to the vector `1:ncol(y)` but if the responses are observed
  at a different spacing (or appear in a different order), that
  information should be used here.

- ...:

  additional arguments passed on to
  [quantreg::rq](https://rdrr.io/pkg/quantreg/man/rq.html). Not all are
  allowed.

## Value

An object of class `smoothqr`

## Examples

``` r
x <- matrix(rnorm(100 * 10), nrow = 100)
y <- matrix(rnorm(100 * 10), nrow = 100)
out <- smooth_qr(x, y, tau = c(.25, .5, .75))
```
