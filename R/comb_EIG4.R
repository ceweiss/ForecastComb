#' @title Trimmed Bias-Corrected Eigenvector Forecast Combination
#'
#' @description Computes forecast combination weights according to the trimmed bias-corrected  eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The underlying methodology of the trimmed bias-corrected eigenvector approach by Hsiao and Wan (2014) is the same as their
#' \code{\link[=comb_EIG2]{bias-corrected eigenvector approach}}. The only difference is that the bias-corrected trimmed eigenvector approach
#' pre-selects the models that serve as input for the forecast combination, only a subset of the available forecast models is retained,
#' while the models with the worst performance are discarded.
#'
#' The number of retained forecast models is controlled via \code{ntop_pred}. The user can choose whether to select this number, or leave the selection
#' to the inbuilt optimization algorithm (in that case \code{ntop_pred = NULL}). If the optimization algorithm should select the best number of
#' retained models, the user must select the optimization \code{criterion}: MAE, MAPE, or RMSE. After this trimming step, the weights, the intercept and the
#' combined forecast are computed in the same way as in the \code{\link[=comb_EIG2]{bias-corrected eigenvector approach}}.
#'
#' The bias-corrected trimmed eigenvector approach combines the strengths of the \cr
#' \code{\link[=comb_EIG2]{bias-corrected eigenvector approach}} and the \code{\link[=comb_EIG3]{trimmed eigenvector approach}}.
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param ntop_pred Specifies the number of retained predictors. If \code{NULL} (default), the inbuilt optimization algorithm selects this number.
#' @param criterion If \code{ntop_pred} is not specified, a selection criterion is required for the optimization algorithm: one of "MAE", "MAPE",
#' or "RMSE". If \code{ntop_pred} is selected by the user, \code{criterion} should be set to \code{NULL} (default).
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the used forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Intercept}{Returns the intercept (bias correction).}
#' \item{Weights}{Returns the combination weights obtained by applying the combination method to the training set.}
#' \item{Top_Predictors}{Number of retained predictors.}
#' \item{Ranking}{Ranking of the predictors that determines which models are removed in the trimming step.}
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
#' ## Number of retained models selected by the user:
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_EIG4(data, ntop_pred = 2, criterion = NULL)
#'
#' ## Number of retained models selected by algorithm:
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' comb_EIG4(data, ntop_pred = NULL, criterion = "RMSE")
#'
#' @seealso
#' \code{\link{comb_EIG2}}
#' \code{\link{comb_EIG3}}
#' \code{\link[GeomComb]{foreccomb}},
#' \code{\link[GeomComb]{plot.foreccomb_res}},
#' \code{\link[GeomComb]{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export
comb_EIG4 <- function(x, ntop_pred = NULL, criterion = "RMSE") {
    pckg <- c("forecast")
    temp <- unlist(lapply(pckg, require, character.only = TRUE))
    if (!all(temp == 1))
        stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call. = FALSE)

    if (class(x) != "foreccomb")
        stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
    observed_vector <- x$Actual_Train
    prediction_matrix <- x$Forecasts_Train
    modelnames <- x$modelnames

    if (!is.null(ntop_pred)) {
        if (!(ntop_pred >= 1) || !((ntop_pred%%1) == 0) || !(ntop_pred <= ncol(prediction_matrix))) {
            stop("Trimmed eigenvector combination requires either a positive integer between
           [1, number of predictors], specifying the number of predictors to retain, or
           a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').",
                call. = FALSE)
        }

        eig4_res <- comp.EIG4(observed_vector, prediction_matrix, ntop_pred)

        intercept <- eig4_res$intercept
        weights <- eig4_res$weights
        fitted <- eig4_res$fitted
        accuracy_insample <- eig4_res$accuracy_insample
        ranking <- eig4_res$ranking

    } else if (!is.null(criterion) && (criterion %in% c("RMSE", "MAE", "MAPE"))) {
        message("Optimization algorithm chooses number of retained models for trimmed bias-corrected eigenvector approach...")
        iter <- ncol(prediction_matrix)

        ntop_pred <- 1
        interm <- comp.EIG4(observed_vector, prediction_matrix, ntop_pred)
        best_so_far <- interm

        for (i in 2:iter) {
            interm <- comp.EIG4(observed_vector, prediction_matrix, ntop_pred = i)
            if (interm$accuracy_insample[, criterion] < best_so_far$accuracy_insample[, criterion]) {
                best_so_far <- interm
                ntop_pred <- i
            }

            intercept <- best_so_far$intercept
            weights <- best_so_far$weights
            fitted <- best_so_far$fitted
            accuracy_insample <- best_so_far$accuracy_insample
            ranking <- best_so_far$ranking
        }
        message(paste0("Algorithm finished. Optimized number of retained models: ", ntop_pred))
    } else stop("Trimmed eigenvector combination requires either a positive integer between
           [1, number of predictors], specifying the number of predictors to retain, or
           a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').",
        call. = FALSE)

    if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
        result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights,
            Top_Predictors = ntop_pred, Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Input_Data = list(Actual_Train = x$Actual_Train,
                Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
        rownames(result$Accuracy_Train) <- "Training Set"
    }

    if (is.null(x$Forecasts_Test) == FALSE) {
        newpred_matrix <- x$Forecasts_Test
        pred <- as.vector(as.vector(intercept) + newpred_matrix %*% weights)
        if (is.null(x$Actual_Test) == TRUE) {
            result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights,
                Top_Predictors = ntop_pred, Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train,
                  Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
        } else {
            newobs_vector <- x$Actual_Test
            accuracy_outsample <- accuracy(pred, newobs_vector)
            result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights,
                Top_Predictors = ntop_pred, Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred, Accuracy_Test = accuracy_outsample,
                Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)),
                class = c("foreccomb_res"))
            rownames(result$Accuracy_Train) <- "Training Set"
            rownames(result$Accuracy_Test) <- "Test Set"
        }
    }
    return(result)
}

comp.EIG4 <- function(observed_vector, prediction_matrix, ntop_pred) {
    sum_sq_error <- colSums((observed_vector - prediction_matrix)^2)
    ranking <- rank(sum_sq_error)
    filter_vec <- ranking <= ntop_pred
    adj_prediction_matrix <- as.matrix(prediction_matrix[, filter_vec])
    mean_obs <- mean(observed_vector)
    mean_preds <- colMeans(adj_prediction_matrix)
    centered_obs <- observed_vector - mean_obs
    centered_preds <- scale(adj_prediction_matrix, scale = FALSE)
    omega_matrix <- t(centered_obs - centered_preds) %*% (centered_obs - centered_preds)/length(observed_vector)
    eigen_decomp <- eigen(omega_matrix)
    ds <- colSums(eigen_decomp$vectors)
    adj_eigen_vals <- eigen_decomp$values/(ds^2)
    min_idx <- which.min(adj_eigen_vals)

    weights <- numeric(length(ranking))
    weights[filter_vec] <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
    intercept <- mean_obs - t(mean_preds) %*% weights[filter_vec]
    ranking <- ranking
    fitted <- as.vector(as.vector(intercept) + prediction_matrix %*% weights)
    accuracy_insample <- accuracy(fitted, observed_vector)

    return(list(intercept = intercept, weights = weights, fitted = fitted, accuracy_insample = accuracy_insample, ranking = ranking))
}
