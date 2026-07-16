# Create a matrix of leads and lags

Create a matrix of leads and lags

## Usage

``` r
lagmat(x, lags)
```

## Arguments

- x:

  a univariate signal, observed regularly

- lags:

  a vector of positive (lags) and negative (leads) numbers

## Value

a matrix

## Examples

``` r
x <- 1:10
lagmat(x, c(-2, 0, 1, 2, 3))
#>       ahead2 lag0 lag1 lag2 lag3
#>  [1,]      1   NA   NA   NA   NA
#>  [2,]      2   NA   NA   NA   NA
#>  [3,]      3    1   NA   NA   NA
#>  [4,]      4    2    1   NA   NA
#>  [5,]      5    3    2    1   NA
#>  [6,]      6    4    3    2    1
#>  [7,]      7    5    4    3    2
#>  [8,]      8    6    5    4    3
#>  [9,]      9    7    6    5    4
#> [10,]     10    8    7    6    5
#> [11,]     NA    9    8    7    6
#> [12,]     NA   10    9    8    7
#> [13,]     NA   NA   10    9    8
#> [14,]     NA   NA   NA   10    9
#> [15,]     NA   NA   NA   NA   10
```
