# binscatteR

## Make Binned Scatterplots

This package makes binned scatter plots. They're all the rage these days!

### How to install

Using the **devtools** package:

``` r
devtools::install_github("shommazumder/binscatteR")
```

### Using the Function

This package is just one function `binscatter`. Here's how to use it:

```r
binnedout <- binscatter(
  data = df,
  x = "x",
  y = "y"
)
```

The function will automatically detect if the independent variable is discrete or continuous for you. Optional arguments such as `numbins`, `se`, and `smooth` control the number of bins to use for continuous variables, estimate/plot standard errors for the binned means, and plot the loess smoother respectively.