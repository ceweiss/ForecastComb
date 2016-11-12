#' PLACEHOLDER for comb_invW
#'
#' Computes forecast combination weights according to the standard eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The standard eigenvector approach retrieves combination weights from the sample estimated mean squared prediction error matrix
#' as follows:
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class 'foreccomb_res'
#' \itemize{
#' \item Method Returns the used forecast combination method.
#' \item Models Returns the individual input models that were used for the forecast combinations.
#' \item Weights Returns the combination weights obtained by applying the combination method to the training set.
#' \item Fitted Returns the fitted values of the combination method for the training set.
#' \item Accuracy_Train Returns range of summary measures of the forecast accuracy for the training set.
#' \item Forecasts_Test Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.
#' \item Accuracy_Test Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.
#' }
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
#' ev_comb_EIG1(data)
#'
#' @seealso
#' \code{\link[GeomComb]{foreccomb}},
#' \code{\link[GeomComb]{plot.foreccomb_res}},
#' \code{\link[GeomComb]{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords ts
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
