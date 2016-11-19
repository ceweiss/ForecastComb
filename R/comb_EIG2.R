#' @title Bias-Corrected Eigenvector Forecast Combination
#'
#' @description Computes forecast combination weights according to the bias-corrected eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The bias-corrected eigenvector approach builds on the idea that if one or more of the predictive models yield biased predictions,
#' the accuracy of the standard eigenvector approach can be improved by eliminating the bias. The optimization procedure to
#' obtain combination weights coincides with the \code{\link[=comb_EIG1]{standard eigenvector approach}}, except
#' that it is applied to the centered MSPE matrix after extracting the bias (by subtracting the column means of the MSPE).
#'
#' The combination weights are calculated as:
#'
#'
#' \deqn{\mathbf{w}^{EIG2} = \frac{1}{\tilde{d}_l} \tilde{\mathbf{w}}^l}{w = (1 / \tilde{d}_l) * \tilde{w}^l}
#'
#' where \eqn{\tilde{d}_j} and \eqn{\tilde{\mathbf{w}}^j}{\tilde{w}^j} are defined analogously to \eqn{d_j} and \eqn{\mathbf{w}^j}{w^j}
#' in the \code{\link[=comb_EIG1]{standard eigenvector approach}}, with the only difference that they correspond to the spectral decomposition of the
#' centered MSPE matrix rather than the uncentered one.
#'
#' The combined forecast is then obtained by:
#'
#' \deqn{\hat{y}_t = a + {\mathbf{f}_t}'\mathbf{w}^{EIG2}}{\hat{y}_t = a + (f_t)'w}
#'
#' where \eqn{a = E(y_t) - E(\mathbf{f}_t)' \mathbf{w}^{EIG2}}{a = E(y_t) - E(f_t)'w} is the intercept for bias correction. If the actual
#' series and the forecasts are stationary, the expectations can be approximated by the time series means, i.e. the intercept is obtained
#' by subtracting the weighted sum of column means of the MSPE matrix from the mean of the actual series. Forecast combination methods
#' including intercepts therefore usually require stationarity.
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the used forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Intercept}{Returns the intercept (bias correction).}
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
#' comb_EIG2(data)
#'
#' @seealso
#' \code{\link{comb_EIG1}},
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{accuracy}}
#'
#' @references
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export
comb_EIG2 <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    mean_obs <- mean(observed_vector)
    mean_preds <- colMeans(prediction_matrix)
    centered_obs <- observed_vector - mean_obs
    centered_preds <- scale(prediction_matrix, scale = FALSE)
    omega_matrix <- t(centered_obs - centered_preds) %*% (centered_obs - centered_preds)/length(observed_vector)
    eigen_decomp <- eigen(omega_matrix)
    ds <- colSums(eigen_decomp$vectors)
    adj_eigen_vals <- eigen_decomp$values/(ds^2)
    min_idx <- which.min(adj_eigen_vals)

    weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
    intercept <- as.numeric(mean_obs - t(mean_preds) %*% weights)
    fitted <- as.vector(as.vector(intercept) + prediction_matrix %*% weights)
    accuracy_insample <- accuracy(fitted, observed_vector)
    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
            Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(as.vector(intercept) + newpred_matrix %*% weights)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted,
                Accuracy_Train = accuracy_insample, Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)),
                class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted,
                Accuracy_Train = accuracy_insample, Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train,
                  Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }

    return(result)
}
