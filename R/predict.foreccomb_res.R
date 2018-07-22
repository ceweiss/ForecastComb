#' @name predict.foreccomb_res
#' @title Prediction function for Forecast Combinations
#' @description \code{prediction} method for class \sQuote{foreccomb_res}. Uses the previously created forecast combination
#' result to predict the combination for a newly provided prediction dataset. 
#'
#' @param object An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param newobs A vector or univariate time series; contains \sQuote{actual values} if a test set is used (optional).
#' @param newpreds A matrix or multivariate time series; contains individual model forecasts if a test set is used (optional). Does not
#' require specification of \code{newobs} -- in the case in which a forecaster only wants to train the forecast combination method
#' with a training set and apply it to future individual model forecasts, only \code{newpreds} is required, not \code{newobs}.
#' @param simplify logical. The default (\code{TRUE}) returns the predictions separately. If set to (\code{FALSE}) the predictions are incorporated
#' into the foreccomb_res object, that is, the object is equal to the one that would have been obtained, if the new prediction set would have
#' been provided when the forecast combination method was trained originally.
#' @param byrow logical. The default (\code{FALSE}) assumes that each column of the forecast matrices (\code{prediction_matrix} and -- if
#' specified -- \code{newpreds}) contains forecasts from one forecast model; if each row of the matrices contains forecasts from
#' one forecast model, set to \code{TRUE}.
#' @param ... potential further arguments (require by generic)
#'
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p)
#' fit<-comb_BG(data)
#' predict(fit, test_p)
#'
#' @seealso
#' \code{\link[ForecastComb]{foreccomb}},
#'
#' @author Christoph E. Weiss and Gernot R. Roetzer
#'
#' @import forecast
#'
#' @rdname predict.foreccomb_res
#' @method predict foreccomb_res
#' @export
predict.foreccomb_res <- function(object, newpreds, newobs = NULL, simplify=TRUE, byrow=FALSE, ...) {
 
  if (!is.null(newpreds)) {
    if (byrow==TRUE){
      newpreds <- t(as.matrix(newpreds))
    }
    
    if (ncol(object$Input_Data$Forecasts_Train) != ncol(newpreds))
      stop("Test set predictions and training set predictions must contain same individual forecasts. Number of forecasts differ.", call. = FALSE)
  }
  
  if (sum(is.na(newpreds)) > 0) {
    stop("Calling predict with newpreds containing NAs is not yet supported. Please use the classical foreccomb object approach.", call. = FALSE)
  }
  
  if(!is.null(newobs) & sum(is.null(newobs)) > 0) {
    stop("New observations for the test set cannot contain NAs.")
  }
  
  if (!is.ts(newpreds)) {
    newpreds <- stats::ts(newpreds)
  }

  colnames(newpreds) <- colnames(object$Input_Data$Forecasts_Train)
  
  if (is.null(object$Predict)) {
    pred <- stats::ts(as.vector(newpreds %*% object$Weights))
  } else {
    pred <- object$Predict(object, newpreds)
  }
  
  attributes(pred)$tsp <- attributes(newpreds)$tsp
  
  if (simplify) {
    result <- pred
  } else {
    object[['Forecasts_Test']] <- pred
    object$Input_Data[['Forecasts_Test']] <- newpreds
    
    if (!is.null(newobs)) {
      accuracy_outsample <- accuracy(pred, newobs)
      object[['Accuracy_Test']] <- accuracy_outsample
      rownames(result$Accuracy_Test) <- "Test Set"
    }
    
    result <- object
  }
  
  return(result)
}