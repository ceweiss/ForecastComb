#' @title Standard Eigenvector Forecast Combination
#'
#' @description Computes forecast combination weights according to the standard eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The standard eigenvector approach retrieves combination weights from the sample estimated mean squared prediction error matrix
#' as follows:
#' Suppose \eqn{y_t} is the variable of interest, there are \eqn{N} not perfectly collinear predictors,
#' \eqn{\mathbf{f}_t = (f_{1t}, \ldots, f_{Nt})'}{f_t = (f_{1t}, \ldots, f_{Nt})'}, \eqn{\Sigma} is the (positive definite)
#' mean squared prediction error matrix of \eqn{\mathbf{f}_t}{f_t} and \eqn{\mathbf{e}}{e} is an \eqn{N \times 1}{N * 1} vector of \eqn{(1, \ldots, 1)'}.
#' The \eqn{N} positive eigenvalues are then arranged in increasing order \eqn{(\Phi_1 = \Phi_{min}, \Phi_2, \ldots, \Phi_N)}, and \eqn{\mathbf{w^j}}{w^j}
#' is defined as the eigenvector corresponding to \eqn{\Phi_j}. The combination weights \eqn{\mathbf{w}^{EIG1} = (w_1, \ldots, w_N)'}{w = (w_1, \ldots, w_N)'} are then
#' chosen corresponding to the minimum of \eqn{\left(\frac{\Phi_1}{d_1^2}, \frac{\Phi_2}{d_2^2},\ldots,\frac{\Phi_N}{d_N^2}\right)}{(\Phi_1/d_1^2, \Phi_2/d_2^2, \ldots,
#' \Phi_N/d_N^2)}, denoted as \eqn{\mathbf{w}^l}{w^l}, where \eqn{d_j = \mathbf{e}'\mathbf{w}^j}{d_j = e'w^j}, as:
#' \deqn{\mathbf{w}^{EIG1} = \frac{1}{d_l} \mathbf{w}^l}{w = 1/d_l w^l}
#'
#' The combined forecast is then obtained by:
#'
#' \deqn{\hat{y}_t = {\mathbf{f}_{t}}'\mathbf{w}^{EIG1}}{\hat{y}_t = (f_t)'w}
#'
#' The difference to extant methods that minimize the population mean squared prediction error (e.g., Newbold and Granger, 1974) is the normalization function. While
#' previous approaches optimize MSPE under the constraint of \eqn{\mathbf{e}'\mathbf{w} = 1}{e'w = 1}, Hsiao and Wan (2014) show that this is dominated by
#' using \eqn{\mathbf{w}'\mathbf{w} = 1}{w'w = 1} as constraint in the optimization problem.
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
#' comb_EIG1(data)
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{comb_NG}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
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
comb_EIG1 <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    error_matrix <- observed_vector - prediction_matrix
    sample_msqu_pred_error <- (t(error_matrix) %*% error_matrix)/length(observed_vector)
    eigen_decomp <- eigen(sample_msqu_pred_error)
    ds <- colSums(eigen_decomp$vectors)
    adj_eigen_vals <- eigen_decomp$values/(ds^2)
    min_idx <- which.min(adj_eigen_vals)

    weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
    fitted <- as.vector(prediction_matrix %*% weights)
    accuracy_insample <- accuracy(fitted, observed_vector)
    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
            Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(newpred_matrix %*% weights)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
