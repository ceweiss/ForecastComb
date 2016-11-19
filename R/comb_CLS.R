#' @title Constrained Least Squares Forecast Combination
#'
#' @description Computes forecast combination weights using constrained least squares (CLS) regression.
#'
#' @details
#' The function is a wrapper around the constrained least squares (CLS) forecast combination implementation of the
#' \emph{ForecastCombinations} package.
#'
#' Compared to the \code{\link[=comb_OLS]{ordinary least squares forecast combination}} method, CLS forecast combination has the additional
#' requirement that the weights, \eqn{\mathbf{w}^{CLS} = (w_1, \ldots, w_N)'}, sum up to 1 and that there is no intercept. That is,
#' the combinations of \code{comb_CLS} are affine combinations.
#'
#' This method was first introduced by Granger and Ramanathan (1984). The general appeal of the method is its ease of interpretation (the
#' weights can be interpreted as percentages) and often produces better forecasts than the OLS method when the individual forecasts are
#' highly correlated. A disadvantage is that if one or more individual forecasts are biased, this bias is not corrected through the
#' forecast combination due to the lack of an intercept.
#'
#' In addition to the version presented by Granger and Ramanathan (1984), this variant of the method adds the restriction that combination
#' weights must be non-negative, which has been found to be almost always outperform unconstrained OLS by Aksu and Gunter (1992) and
#' was combined with the condition of forcing the weights to sum up to one by Nowotarski et al. (2014), who conclude that even though the
#' method provides a suboptimal solution in-sample, it almost always produces better
#' forecasts than unrestricted OLS out-of-sample.
#'
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the best-fit forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights obtained by applying the combination method to the training set.}
#' \item{Fitted}{Returns the fitted values of the combination method for the training set.}
#' \item{Accuracy_Train}{Returns range of summary measures of the forecast accuracy for the training set.}
#' \item{Forecasts_Test}{Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.}
#' \item{Accuracy_Test}{Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.}
#' \item{Input_Data}{Returns the data forwarded to the method.}
#'
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_CLS(data)
#'
#' @seealso
#' \code{\link[ForecastCombinations]{Forecast_comb}},
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Aksu, C., and Gunter, S. I. (1992). An Empirical Analysis of the Accuracy of SA, OLS,
#' ERLS and NRLS Combination Forecasts. \emph{International Journal of Forecasting}, \bold{8(1)}, 27--43.
#'
#' Granger, C., and Ramanathan, R. (1984). Improved Methods Of Combining Forecasts. \emph{Journal of Forecasting}, \bold{3(2)}, 197--204.
#'
#' Nowotarski, J., Raviv, E., Tr\"uck, S., and Weron, R. (2014). An Empirical Comparison of Alternative
#' Schemes for Combining Electricity Spot Price Forecasts. \emph{Energy Economics}, \bold{46}, 395--412.
#'
#' @keywords models
#'
#' @import forecast ForecastCombinations
#'
#' @export
comb_CLS <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    regression <- Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "cls")

    weights <- regression$weights[1:length(regression$weights)]
    fitted <- as.vector(regression$fitted[, 1])
    accuracy_insample <- accuracy(fitted, observed_vector)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
            Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        regression_aux <- Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "cls")
        pred <- as.vector(regression_aux$pred[, 1])
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
