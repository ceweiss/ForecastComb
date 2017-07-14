#' @title All Possible Combinations Forecast Averaging
#'
#' @description Combine different forecasts using complete subset regressions. Apart from the simple averaging, weights based on information criteria (AIC, corrected AIC, Hannan Quinn and BIC).
#'
#' @details
#' OLS forecast combination is based on \deqn{ obs_t = const + \sum_{i = 1}^p w_{i} \widehat{obs}_{it} + e_t, } where \eqn{obs} is the observed values and \eqn{\widehat{obs}} is the forecast, one out of the \eqn{p} forecasts available.
#' 
#' The function computes the complete subset regressions. So a matrix of forecasts based on all possible subsets of \code{fhat} is returned.
#' 
#' Those forecasts can later be cross-sectionally averaged (averaged over rows) to create a single combined forecast using weights which are based on the information criteria of the different individual regression, rather than a simple average.
#' 
#' Additional weight-vectors which are based on different information criteria are also returned. This is in case the user would like to perform the frequensit version of forecast averaging (see references for more details).
#' 
#' Although the function is geared towards forecast averaging, it can be used in any other application as a generic complete subset regression.
#'
#' @param x An object of class \code{foreccomb}. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#'
#' @return Returns an object of class \code{foreccomb_res} with the following components:
#' \item{Method}{Returns the used forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights based on the different information criteria.}
#' \item{Fitted}{Returns the fitted values for each information criterion.}
#' \item{Accuracy_Train}{Returns range of summary measures of the forecast accuracy for the training set.}
#' \item{Forecasts_Test}{Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.}
#' \item{Accuracy_Test}{Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.}
#' \item{Input_Data}{Returns the data forwarded to the method.}
#'
#' @author Eran Raviv  and Gernot R. Roetzer
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
#' comb_CSR(data)
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link{comb_NG}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Hansen, B. (2008). Least-squares forecast averaging \emph{Journal of Econometrics}, \bold{146(2)}, 342--350.
#'
#' Kapetanios, G., Labhard V., Price, S. (2008). Forecasting Using Bayesian and Information-Theoretic Model Averaging.
#' \emph{Journal of Business & Economic Statistics}, \bold{26(1)}.
#' 
#' Koenker R. (2005). \emph{Quantile Regression. Cambridge University Press}.
#'
#' Graham, E., Garganob, A., Timmermann, A. (2013). Complete subset regressions. 
#' \emph{Journal of Econometrics}, \bold{177(2)}, 357--373. 
#'
#' @keywords models
#'
#' @import forecast
#' @importFrom stats fitted.values lm logLik
#' @importFrom utils combn
#' 
#' @export
comb_CSR <- function(x) {
  if (class(x) != "foreccomb")
    stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
  observed_vector <- x$Actual_Train
  prediction_matrix <- x$Forecasts_Train
  modelnames <- x$modelnames
  ic_name_vec <- c("aic", "aicc", "bic",  "hq")
  
  p <- NCOL(prediction_matrix)
  TT <- NROW(prediction_matrix)
  
  ndiff_models <- 2^p - 1
  weights <- matrix(0, ndiff_models, 4)
  indiv_fitted <- matrix(0, length(observed_vector), ndiff_models)
  
  weights_idx <- 1
  indiv_fitted_idx <- 1
  
  conduct_predict <- !is.null(x$Forecasts_Test)
    
  if (conduct_predict) {
    newpred_matrix <- x$Forecasts_Test
    indiv_pred <- matrix(0, NROW(newpred_matrix), ndiff_models)
    indiv_pred_idx <- 1
  }
    
  for (i in 1:p) {
    combinations <- as.matrix(combn(p, i))
    ncombs <- ncol(combinations)
    
    lin_models <- apply(combinations, 2, function(combs, data) {
      lm(observed_vector ~ as.matrix(data)[, combs])
    }, data=prediction_matrix)
    
    end_weights_idx <- weights_idx + ncombs - 1
    weights[weights_idx:end_weights_idx,] <- t(sapply(lin_models, crit_fun))
    weights_idx <- end_weights_idx + 1
    
    end_indiv_fitted_idx <- indiv_fitted_idx + ncombs - 1
    indiv_fitted[, indiv_fitted_idx:end_indiv_fitted_idx] <- sapply(lin_models, FUN = fitted.values)
    indiv_fitted_idx <- end_indiv_fitted_idx + 1
    
    if (conduct_predict) {
      end_indiv_pred_idx <- indiv_pred_idx + ncombs - 1
      indiv_pred[, indiv_pred_idx:end_indiv_pred_idx] <- mapply(comp_predict, lin_models, 1:ncombs, 
                                                                MoreArgs = list(newdata = newpred_matrix, 
                                                                                combinations = combinations))
      indiv_pred_idx <- end_indiv_pred_idx + 1  
    }
  }
  
  weigths <- apply(weights, MARGIN = 2, FUN = comp_normalized_weights)
  colnames(weights) <- ic_name_vec
  
  fitted <- indiv_fitted %*% weigths
  colnames(fitted) <- ic_name_vec

  accuracy_insample <- apply(fitted, MARGIN = 2, FUN = accuracy, x = observed_vector)[1:5,]
  rownames(accuracy_insample) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  colnames(accuracy_insample) <- ic_name_vec
  if(!conduct_predict & is.null(x$Actual_Test)) {
    result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
  }
  
  if(conduct_predict) {
    pred <- indiv_pred %*% weigths
    colnames(pred) <- ic_name_vec
    if(is.null(x$Actual_Test) == TRUE) {
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
    } else {
      newobs_vector <- x$Actual_Test
      accuracy_outsample <- apply(pred, MARGIN = 2, FUN = accuracy, x = newobs_vector)[1:5,]
      rownames(accuracy_outsample) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
      colnames(accuracy_outsample) <- ic_name_vec
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                                                                                                            Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
    }
  }
  
  return(result)
}

# Computes the information criterions for weighting
crit_fun <- function(x) {
  k <- length(x$coef)  # will include a constant
  TT <- length(x$res)
  aic <- as.numeric(-2 * logLik(x) + 2 * k)
  aicc <- aic + 2 * (k + 1) * (k + 2)/(TT - k - 1)
  bic <- as.numeric(-2 * logLik(x) + log(TT) * k)
  hq <- as.numeric(-2 * logLik(x) + log(log(TT)) * k)
  return(c(aic = aic, aicc = aicc, bic = bic, hq = hq))
}

comp_predict <- function(lin_model, comb_idx, newdata, combinations) {
  TT <- NROW(newdata)
  comb <- combinations[,comb_idx]
  pred <- cbind(rep(1, TT), newdata[, comb]) %*% lin_model$coef
  return(pred)
}

comp_normalized_weights <- function(x) {
  x <- scale(unlist(x), scale = T)  # scaling is necessary to escape zero
  nominatorr <- exp(-0.5 * x)
  nominatorr/sum(nominatorr)
}