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

Accuracy measures are provided accordingly, summary and plot functions have
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


