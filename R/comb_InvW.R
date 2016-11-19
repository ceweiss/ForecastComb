#' @title Inverse Rank Forecast Combination
#'
#' @description Computes forecast combination weights according to the inverse rank approach by Aiolfi and Timmermann (2006) and produces forecasts for the test set, if provided.
#'
#' @details
#' In the inverse rank approach by Aiolfi and Timmermann (2006), the combination weights are inversely proportional to the forecast model's rank, \eqn{Rank_i}:
#'
#' \deqn{w_i^{InvW} = \frac{Rank_i^{-1}}{\Sigma_{j=1}^N  Rank_j^{-1}}}{w_i = Rank_i^{-1} / \Sigma_{j=1}^N  Rank_j^{-1}}
#'
#' The combined forecast is then obtained by:
#'
#' \deqn{\hat{y}_t = {\mathbf{f}_{t}}'\mathbf{w}^{InvW}}{\hat{y}_t = (f_t)'w}
#'
#' This is a robust variant of the Bates/Granger (1969) approach and also ignores correlations across forecast errors.
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
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_InvW(data)
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{comb_BG}},
#' \code{\link[forecast]{accuracy}}
#'
#' @author Christoph E. Weiss and Gernot R. Roetzer
#'
#' @references
#' Aiolfi, M., amd Timmermann, A. (2006). Persistence in Forecasting Performance and Conditional Combination Strategies. \emph{Journal of Econometrics}, \bold{135(1)}, 31--53.
#'
#' Bates, J. M., and Granger, C. W. (1969). The Combination of Forecasts. \emph{Journal of the Operational Research Society}, \bold{20(4)}, 451--468.
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export
comb_InvW <- function(x) {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    error_matrix <- observed_vector - prediction_matrix
    sum_sq_error <- colSums((error_matrix)^2)
    ranking <- rank(sum_sq_error)
    inv_ranking <- 1/ranking

    weights <- inv_ranking/sum(inv_ranking)
    fitted <- as.vector(prediction_matrix %*% weights)
    accuracy_insample <- accuracy(fitted, observed_vector)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, Input_Data = list(Actual_Train = x$Actual_Train,
            Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(newpred_matrix %*% weights)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
