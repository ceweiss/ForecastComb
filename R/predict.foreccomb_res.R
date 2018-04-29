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