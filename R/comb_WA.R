#Winsorized Mean (including option for optimization of trim factor based on training set):
comb_WA <- function(x, trim_factor=NULL, criterion=NULL) {
  pckg<-c("forecast", "psych")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"psych\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  if(!is.null(trim_factor)){
    if(!is.numeric(trim_factor)) stop("Trimming Factor must be numeric.", call.=FALSE)
    if(abs(trim_factor)>0.5) stop("Trimming Factor must be between 0 and 0.5.", call.=FALSE)
    trimf<-trim_factor
    adj_pred<-apply(prediction_matrix,1, function(x) winsor.mean(x, trim=trimf, na.rm=TRUE))
  }
  else{
    if (is.null(criterion)) stop("Automatic optimization of trim factor requires selection of 'criterion'.", call.=FALSE)
    if (length(grep(criterion, c("MAE", "MAPE", "RMSE")))!=1) stop("Criterion for trim factor optimization must be 'MAE', 'MAPE', or 'RMSE'.", call.=FALSE)
    aux_matrix<-matrix(NA, nrow=51, ncol=1)
    rownames(aux_matrix)<-seq(0,0.5,0.01)
    for (i in 1:51){
      aux_matrix[i,]<-accuracy(apply(prediction_matrix, 1, function(x) winsor.mean(x, trim=((i-1)/100),na.rm=TRUE)), observed_vector)[2]
    }
    best<-which(aux_matrix==min(aux_matrix))[1]
    trimf<-as.numeric(rownames(aux_matrix)[best])
    
    adj_pred<-apply(prediction_matrix,1, function(x) winsor.mean(x, trim=trimf, na.rm=TRUE))
  }
  
  weights <- "Weights of the individual forecasts differ over time with winsorized mean"
  fitted <- adj_pred
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights,
                             Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)),
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- apply(newpred_matrix, 1, function(x) winsor.mean(x, trim=trimf))
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights,
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights,
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)),
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}
