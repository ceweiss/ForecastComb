#' @title Median Forecast Combination
#'
#' @description Computes a \sQuote{combined forecast} from a pool of individual model forecasts using their median at each point in time.
#'
#' @details
#' Suppose \eqn{y_t} is the variable of interest, there are \eqn{N} not perfectly collinear predictors,
#' \eqn{\mathbf{f}_t = (f_{1t}, \ldots, f_{Nt})'}. For each point in time, the median method gives
#' a weight of 1 to the median forecast and a weight of 0 to all other forecasts, the combined forecast is obtained by:
#'
#' \deqn{\hat{y}_t = {median(\mathbf{f}_{t}})}{\hat{y}_t = median(f_t)}
#'
#' The median method is an appealing simple, rank-based combination method that has been proposed by authors such as Armstrong (1989),
#' McNees (1992), Hendry and Clements (2004), Stock and Watson (2004), and Timmermann (2006). It is more robust to outliers than the
#' simple average approach.
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the used forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights obtained by applying the combination method to the training set.}
#' \item{Fitted}{Returns the fitted values of the combination method for the training set.}
#' \item{Accuracy_Train}{Returns range of summary measures of the forecast accuracy for the training set.}
#' \item{Forecasts_Test}{Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.}
#' \item{Accuracy_Test}{Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.}
#' \item{Input_Data}{Returns the data forwarded to the method.}
#'
#' @author Christoph E. Weiss and Gernot R. Roetzer
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
#' comb_MED(data)
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{comb_SA}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Armstrong, J. S. (1989). Combining Forecasts: The End of the Beginning or the Beginning of the End?. \emph{International Journal of Forecasting},
#' \bold{5(4)}, 585--588.
#'
#' Hendry, D. F., and Clements, M. P. (2004). Pooling of Forecasts. \emph{The Econometrics Journal}, \bold{7(1)}, 1--31.
#'
#' McNees, S. K. (1992). The Uses and Abuses of 'Consensus' Forecasts. \emph{Journal of Forecasting}, \bold{11(8)}, 703--710.
#'
#' Stock, J. H., and Watson, M. W. (2004). Combination Forecasts of Output Growth in a Seven-Country Data Set. \emph{Journal of Forecasting}, \bold{23(6)},
#' 405--430.
#'
#' Timmermann, A. (2006). Forecast Combinations. In: Elliott, G., Granger, C. W. J., and Timmermann, A. (Eds.), \emph{Handbook of Economic Forecasting},
#' \bold{1}, 135--196.
#'
#' @keywords models
#'
#' @import forecast
#' @importFrom stats median
#'
#' @export
comb_MED <- function(x) {
    pckg <- c("forecast")
    temp <- unlist(lapply(pckg, require, character.only = TRUE))
    if (!all(temp == 1))
        stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call. = FALSE)

    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    weights <- "Weights of the individual forecasts differ over time with median method"
    fitted <- apply(prediction_matrix, 1, median)
    accuracy_insample <- accuracy(fitted, observed_vector)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Median Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, Input_Data = list(Actual_Train = x$Actual_Train,
            Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- apply(newpred_matrix, 1, median)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Median Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Median Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
