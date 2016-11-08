#Bias-Corrected Eigenvector Approach:

ev_comb_EIG2 <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  mean_obs <- mean(observed_vector)
  mean_preds <- colMeans(prediction_matrix)
  centered_obs <- observed_vector - mean_obs
  centered_preds <- scale(prediction_matrix, scale=FALSE)
  omega_matrix <- t(centered_obs - centered_preds)%*%(centered_obs - centered_preds)/length(observed_vector)
  eigen_decomp <- eigen(omega_matrix)
  ds <- colSums(eigen_decomp$vectors)
  adj_eigen_vals <- eigen_decomp$values/(ds^2)
  min_idx <- which.min(adj_eigen_vals)
  
  weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
  intercept <- as.numeric(mean_obs - t(mean_preds)%*%weights)
  fitted <- as.vector(as.vector(intercept)+prediction_matrix%*%weights)
  accuracy_insample<-accuracy(fitted,observed_vector)
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Weights = weights, Intercept = intercept, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)),
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(as.vector(intercept)+newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  
  return(result)
}
