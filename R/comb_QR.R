#Robust Regression:
comb_QR <- function(x) {
  pckg<-c("forecast", "ForecastCombinations")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"ForecastCombinations\".\n Use install.packages() if they are not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  regression<-Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "robust")
  
  weights <- regression$weights[2:length(regression$weights)]
  intercept <- regression$weights[1]
  fitted <- as.vector(regression$fitted[,1])
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept,
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)),
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    regression_aux<-Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "robust")
    pred <- as.vector(regression_aux$pred[,1])
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}
