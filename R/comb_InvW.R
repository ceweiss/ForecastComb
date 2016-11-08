#Inverse Ranking Approach:
comb_InvW <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  error_matrix <- observed_vector - prediction_matrix
  sum_sq_error <- colSums((error_matrix)^2)
  ranking <- rank(sum_sq_error)
  inv_ranking <- 1/ranking
  
  weights <- inv_ranking/sum(inv_ranking)
  fitted <- as.vector(prediction_matrix%*%weights)
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights,
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)),
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights,
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
