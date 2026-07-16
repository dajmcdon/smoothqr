# Make predictions from a smoothed multi-output quantile regression

Make predictions from a smoothed multi-output quantile regression

## Usage

``` r
# S3 method for class 'smoothqr'
predict(object, newdata, ...)
```

## Arguments

- object:

  Object of class inheriting from `smooth_qr`

- newdata:

  A matrix or data frame in which to look for variables with which to
  predict. May NOT be omitted.

- ...:

  not used

## Value

a List of predicted values, one matrix per response.

## Examples

``` r
x <- matrix(rnorm(100 * 10), nrow = 100)
y <- matrix(rnorm(100 * 10), nrow = 100)
out <- smooth_qr(x, y, tau = c(.25, .5, .75))
p <- predict(out, newdata = x[1:10, ])
```
