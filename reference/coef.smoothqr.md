# Extract smoothed quantile regression model coefficients

Extract smoothed quantile regression model coefficients

## Usage

``` r
# S3 method for class 'smoothqr'
coef(object, type = c("response", "smoothed"), ...)
```

## Arguments

- object:

  an object for which the extraction of model coefficients is
  meaningful.

- type:

  The option `"response"` means that the smoothing will be inverted,
  giving \\\Theta H'\\. This will result in a List containing one matrix
  for each response. Alternatively, choosing `"smoothed"` will return
  \\\Theta\\ with the List containing one matrix for each degree.

- ...:

  not used.

## Value

a List of coefficient matrices.

## Examples

``` r
x <- matrix(rnorm(100 * 10), nrow = 100)
y <- matrix(rnorm(100 * 10), nrow = 100)
out <- smooth_qr(x, y, tau = c(.25, .5, .75))
cc1 <- coef(out)
cc2 <- coef(out, "smoothed")
```
