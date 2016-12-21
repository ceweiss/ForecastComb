# GeomComb
>(Geometric) Forecast Combination in R

[![Build Status](https://img.shields.io/travis/ceweiss/GeomComb/master.svg)](https://travis-ci.org/ceweiss/GeomComb)
[![Build_Note](http://www.r-pkg.org/badges/version/GeomComb)](https://cran.r-project.org/package=GeomComb)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/GeomComb)](https://cran.r-project.org/package=GeomComb)

The R package *GeomComb* presents functions to pool individual model forecasts
using geometric (eigenvector-based) forecast combination methods. The package
also provides functions for simple forecast combination methods (inverse rank
approach, simple average, median, trimmed mean, and winsorized mean - including 
the option of a criterion-based optimisation of the trimming factor) and 
regression-based forecast combination methods.

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

You can install the **stable** version on [CRAN](https://cran.r-project.org/package=GeomComb):

```s
install.packages('GeomComb', dependencies = TRUE)
```

You can also install the **development** version from
[Github](https://github.com/ceweiss/GeomComb)

```s
# install.packages("devtools")
devtools::install_github("ceweiss/GeomComb")
```

## License

This package is free and open source software, licensed under GPL (>= 2).


