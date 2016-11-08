
summary.foreccomb_res<-function(x, plot=TRUE){
  if(class(x)!="foreccomb_res") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  cat("\n")
  cat("Summary of Forecast Combination \n")
  cat("------------------------------- \n")
  cat("\n")
  Method<-x$Method
  cat("Method: ", Method, "\n")
  cat("\n")
  cat("Individual Forecasts & Combination Weights: \n")
  cat("\n")
  weight<-matrix(x$Weights, ncol=1)
  colnames(weight)<-"Combination Weight"
  rownames(weight)<-x$Models
  print(weight)
  cat("\n")
  if (!is.null(x$Intercept)){
    cat("Intercept (Bias-Correction): ", x$Intercept, "\n")
    cat("\n")
  }
  cat("Accuracy of Combined Forecast: \n")
  cat("\n")
  accuracy<-rbind(x$Accuracy_Train[1:5], x$Accuracy_Test)
  rownames(accuracy)[1]<-"Training Set"
  print(accuracy)
  cat("\n")
  cat("Additional information can be extracted from the combination object: \n")
  cat("For fitted values (training set): ", paste0(deparse(substitute(x)), "$Fitted"), "\n")
  if(!is.null(x$Forecasts_Test)){
    cat("For forecasts (test set): ", paste0(deparse(substitute(x)), "$Forecasts_Test"), "\n")
  }
  cat("See ", paste0("str(", deparse(substitute(x)), ")"), " for full list.")
  
  if(plot==TRUE){
    plot(x)
  }
}
