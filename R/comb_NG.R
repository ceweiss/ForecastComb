#' @title Newbold/Granger (1974) Forecast Combination
#'
#' @description Computes forecast combination weights according to the approach by Newbold and Granger (1974) and produces forecasts for the test set, if provided.
#'
#' @details
#' Building on early research by Bates and Granger (1969), the methodology of Newbold and Granger (1974) also extracts the combination weights from the estimated
#' mean squared prediction error matrix.
#'
#' Suppose \eqn{y_t} is the variable of interest, there are \eqn{N} not perfectly collinear predictors,
#' \eqn{\mathbf{f}_t = (f_{1t}, \ldots, f_{Nt})'}{f_t = (f_{1t}, \ldots, f_{Nt})'}, \eqn{\Sigma} is the (positive definite)
#' mean squared prediction error matrix of \eqn{\mathbf{f}_t}{f_t} and \eqn{\mathbf{e}}{e} is an \eqn{N \times 1}{N * 1} vector of \eqn{(1, \ldots, 1)'}.
#'
#' Their approach is a constrained minimization of the mean squared prediction error using the normalization condition \eqn{\mathbf{e}'\mathbf{w} = 1}{e'w = 1}.
#' This yields the following combination weights:
#'
#' \deqn{\mathbf{w}^{NG} = \frac{\Sigma^{-1}\mathbf{e}}{\mathbf{e}'\Sigma^{-1}\mathbf{e}}}{w = (\Sigma^{-1} * e) / (e' * \Sigma^{-1} * e)}
#'
#' The combined forecast is then obtained by:
#'
#' \deqn{\hat{y}_t = {\mathbf{f}_{t}}'\mathbf{w}^{NG}}{\hat{y}_t = (f_t)'w}
#'
#' While the method dates back to Newbold and Granger (1974), the variant of the method used here does not impose the prior restriction that \eqn{\Sigma}
#' is diagonal. This approach, called \code{VC} in Hsiao and Wan (2014), is a generalization of the original method.
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
#' comb_NG(data)
#'
#' @seealso
#' \code{\link{comb_BG}},
#' \code{\link{comb_EIG1}},
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Bates, J. M., and Granger, C. W. (1969). The Combination of Forecasts. \emph{Journal of the Operational Research Society}, \bold{20(4)}, 451--468.
#'
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' Newbold, P., and Granger, C. W. J. (1974). Experience with Forecasting Univariate Time Series and the Combination of Forecasts.
#' \emph{Journal of the Royal Statistical Society, Series A}, \bold{137(2)}, 131--165.
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export
comb_NG <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    error_matrix <- observed_vector - prediction_matrix
    sample_msqu_pred_error <- (t(error_matrix) %*% error_matrix)/length(observed_vector)
    e_vec <- as.matrix(rep(1, ncol(prediction_matrix)))

    weights <- as.vector(solve(sample_msqu_pred_error) %*% e_vec)/as.numeric(t(e_vec) %*% solve(sample_msqu_pred_error) %*% e_vec)
    fitted <- as.vector(prediction_matrix %*% weights)
    accuracy_insample <- accuracy(fitted, observed_vector)
    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Newbold/Granger (1974)", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, Input_Data = list(Actual_Train = x$Actual_Train,
            Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(newpred_matrix %*% weights)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Newbold/Granger (1974)", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Newbold/Granger (1974)", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
