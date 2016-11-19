#' @title Trimmed Mean Forecast Combination
#'
#' @description Computes a \sQuote{combined forecast} from a pool of individual model forecasts using trimmed mean at each point in time.
#'
#' @details
#' Suppose \eqn{y_t} is the variable of interest, there are \eqn{N} not perfectly collinear predictors,
#' \eqn{\mathbf{f}_t = (f_{1t}, \ldots, f_{Nt})'}{f_t = (f_{1t}, \ldots, f_{Nt})'}. For each point in time, the order forecasts are
#' computed:
#'
#' \deqn{\mathbf{f}_t^{ord} = (f_{(1)t}, \ldots, f_{(N)t})'}{(f_t)_ord = (f_{(1)t}, \ldots, f_{(N)t})'}
#'
#' Using a trim factor \eqn{\lambda} (i.e., the top/bottom \eqn{\lambda \%} are trimmed) the combined forecast is calculated as:
#'
#' \deqn{\hat{y}_t = \frac{1}{N(1-2\lambda)} \sum_{i = \lambda N +1}^{(1-\lambda)N} f_{(i)t}}{\hat{y}_t = 1/(N*(1-2\lambda)) \sum_{i = \lambda N +1}^{(1-\lambda)N} f_{(i)t}}
#'
#' The trimmed mean is an interpolation between the simple average and the median. It is an appealing simple, rank-based
#' combination method that is less sensitive to outliers than the simple average approach, and has been proposed by authors
#' such as Armstrong (2001), Stock and Watson (2004), and Jose and Winkler (2008).
#'
#' This method allows the user to select \eqn{\lambda} (by specifying \code{trim_factor}), or to leave the selection to
#' an optimization algorithm -- in which case the optimization criterion has to be selected (one of "MAE", "MAPE", or "RMSE").
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param trim_factor numeric. Must be between 0 (simple average) and 0.5 (median).
#' @param criterion If \code{trim_factor} is not specified, an optimization criterion for automated trimming needs to be defined. One of
#' "MAE", "MAPE", or "RMSE" (default).
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the used forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights obtained by applying the combination method to the training set.}
#' \item{Trim Factor}{Returns the trim factor, \eqn{\lambda}.}
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
#' ## User-selected trim factor:
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_TA(data, trim_factor=0.1)
#'
#' ## Algorithm-optimized trim factor:
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_TA(data, criterion="RMSE")
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{comb_SA}},
#' \code{\link{comb_MED}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Armstrong, J. S. (2001). Combining Forecasts. In: \emph{Armstrong, J. S. (Ed.), Principles of Forecasting. Springer, Boston, MA}, 417--439.
#'
#' Jose, V. R. R., and Winkler, R. L. (2008). Simple Robust Averages of Forecasts: Some Empirical Results. \emph{International Journal of Forecasting}, \bold{24(1)}, 163--169.
#'
#' Stock, J. H., and Watson, M. W. (2004). Combination Forecasts of Output Growth in a Seven-Country Data Set. \emph{Journal of Forecasting}, \bold{23(6)},
#' 405--430.
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export
comb_TA <- function(x, trim_factor = NULL, criterion = "RMSE") {
    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    if (!is.null(trim_factor)) {
        if (!is.numeric(trim_factor))
            stop("Trim Factor must be numeric.", call. = FALSE)
        if (abs(trim_factor) > 0.5)
            stop("Trim Factor must be between 0 and 0.5.", call. = FALSE)
        trimf <- trim_factor
        adj_pred <- apply(prediction_matrix, 1, function(x) mean(x, trim = trimf, na.rm = TRUE))
    } else {
        if (is.null(criterion))
            stop("Automatic optimization of trim factor requires selection of 'criterion'.", call. = FALSE)
        if (length(grep(criterion, c("MAE", "MAPE", "RMSE"))) != 1)
            stop("Criterion for trim factor optimization must be 'MAE', 'MAPE', or 'RMSE'.", call. = FALSE)
        aux_matrix <- matrix(NA, nrow = 51, ncol = 1)
        rownames(aux_matrix) <- seq(0, 0.5, 0.01)
        message("Optimization algorithm chooses trim factor for trimmed mean approach...")
        for (i in 1:51) {
            if (criterion == "RMSE")
                aux_matrix[i, ] <- accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim = ((i - 1)/100), na.rm = TRUE)), observed_vector)[2]
            if (criterion == "MAE")
                aux_matrix[i, ] <- accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim = ((i - 1)/100), na.rm = TRUE)), observed_vector)[3]
            if (criterion == "MAPE")
                aux_matrix[i, ] <- accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim = ((i - 1)/100), na.rm = TRUE)), observed_vector)[5]
        }
        best <- which(aux_matrix == min(aux_matrix))[1]
        trimf <- as.numeric(rownames(aux_matrix)[best])

        message(paste0("Algorithm finished. Optimized trim factor: ", trimf))

        adj_pred <- apply(prediction_matrix, 1, function(x) mean(x, trim = trimf, na.rm = TRUE))
    }

    weights <- "Weights of the individual forecasts differ over time with trimmed mean"
    fitted <- adj_pred
    accuracy_insample <- accuracy(fitted, observed_vector)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights, Trim_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
            Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- apply(newpred_matrix, 1, function(x) mean(x, trim = trimf))
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights, Trim_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights, Trim_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
                Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                  Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}
