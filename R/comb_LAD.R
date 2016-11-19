#' @title Least Absolute Deviation Forecast Combination
#'
#' @description Computes forecast combination weights using least absolute deviation (LAD) regression.
#'
#' @details
#' The function is a wrapper around the least absolute deviation (LAD) forecast combination implementation of the
#' \emph{ForecastCombinations} package.
#'
#' The defining property of \code{comb_LAD} is that it does not minimize the squared error loss like \code{\link{comb_OLS}} and
#' \code{\link{comb_CLS}}, but the absolute values of the errors. This makes the method more robust to outliers -- \code{comb_LAD}
#' tends to penalize models, which have high errors for some observations, less harshly than the least squares methods would.
#'
#' Optimal forecast combinations under general loss functions are discussed by Elliott and Timmermann (2004). The LAD method is
#' described in more detail, and used in an empirical context, by Nowotarksi et al. (2014).
#'
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the best-fit forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights obtained by applying the combination method to the training set.}
#' \item{Intercept}{Returns the intercept of the linear regression.}
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
#' comb_LAD(data)
#'
#' @references
#' Elliott, G., and Timmermann, A. (2004). Optimal Forecast Combinations Under General Loss Functions and Forecast Error Distributions.
#' \emph{Journal of Econometrics}, \bold{122(1)}, 47--79.
#'
#' Nowotarski, J., Raviv, E., Tr\"uck, S., and Weron, R. (2014). An Empirical Comparison of Alternative
#' Schemes for Combining Electricity Spot Price Forecasts. \emph{Energy Economics}, \bold{46}, 395--412.
#'
#' @seealso
#' \code{\link[ForecastCombinations]{Forecast_comb}},
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @keywords models
#'
#' @import forecast ForecastCombinations
#'
#' @export
comb_LAD <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    regression <- Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "robust")

    weights <- regression$weights[2:length(regression$weights)]
    intercept <- regression$weights[1]
    fitted <- as.vector(regression$fitted[, 1])
    accuracy_insample <- accuracy(fitted, observed_vector)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept, Fitted = fitted, Accuracy_Train = accuracy_insample,
            Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        regression_aux <- Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "robust")
        pred <- as.vector(regression_aux$pred[, 1])
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
