# GeomComb
>(Geometric) Forecast Combination in R

(written by Chris E. Weiss and Gernot Roetzer)

The R package *GeomComb* presents functions to pool individual model forecasts
using geometric (eigenvector-based) forecast combination methods. The package
also provides functions for simple forecast combination methods (inverse rank
approach, simple average, trimmed mean, and winsorized mean - including the 
option of a criterion-based optimisation of the trimming factor) and 
regression-based forecast combination methods.

The forecast combination methods allow for 3 different input types:

1) Only training set

2) Training set + future forecasts

3) Full training + test set

Using S3 classes, the raw input data is transformed into a 'foreccomb' object.
This initial data cleaning step makes sure that the data is passed on to the
combination methods in the correct format, but it also allows the user to
choose how to handle missing values (the options are to either omit them or
impute them using multivariate time series imputation methods), and runs a
test for perfect collinearity to avoid problems at the combination stage.

The clean data is then passed on to the desired combination method. The output
includes the combination weights, fitted values (and forecasts if a test set
was used), as well as accuracy evaluation. Summary and plot functions have
been created for the S3 classes. The function auto.combine() is an automated
selection of the best combination method based on criterion optimisation in
the training set.

## Installation
The package is still in the development stage -- updates on CRAN release will
be shared here in the future.

If you are interested in using the provided functions for your research in the
meantime, you are welcome to email us: info@ceweiss.com

You can also install the **development** version from
[Github](https://github.com/ceweiss/GeomComb)

```s
# install.packages("devtools")
devtools::install_github("ceweiss/GeomComb")
```

## License

This package is free and open source software, licensed under GPL (>= 2).


