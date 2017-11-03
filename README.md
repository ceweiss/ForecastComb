# ForecastComb
>Forecast Combination in R

[![Build Status](https://img.shields.io/travis/ceweiss/ForecastComb/master.svg)](https://travis-ci.org/ceweiss/ForecastComb)
[![Build_Note](http://www.r-pkg.org/badges/version/ForecastComb)](https://cran.r-project.org/package=ForecastComb)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ForecastCombinations)](https://cran.r-project.org/package=ForecastComb)

The R package *ForecastComb*  presents functions to pool individual model forecasts
using geometric- and regression-based forecast combination methods.  *ForecastComb* combines the functionality of the packages *ForecastCombinations* and *GeomComb* under a unified user interface and convenience functions.

The forecast combination methods allow for 3 different input types:

1) Only training set

2) Training set + future forecasts

3) Full training + test set

Accuracy measures are provided accordingly, summary and plot functions have
been created for the S3 classes. The function auto.combine() is an automated
selection of the best combination method based on criterion optimisation in
the training set.

## Installation
Get started by installing the [R software](https://www.r-project.org/) for statistical computing.

You can install the **stable** version on [CRAN](https://cran.r-project.org/package=ForecastComb):

```s
install.packages('ForecastComb', dependencies = TRUE)
```

You can also install the **development** version from
[Github](https://github.com/ceweiss/ForecastComb)

```s
# install.packages("devtools")
devtools::install_github("ceweiss/ForecastComb")
```

## License

This package is free and open source software, licensed under GPL (>= 2).


