
summary.foreccomb_res<-function(x){
  if(class(x)!="foreccomb_res") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  ans <- list()
  
  ans$Method<-x$Method
  
  ans$weight<-matrix(x$Weights, ncol=1)
  colnames(ans$weight)<-"Combination Weight"
  rownames(ans$weight)<-x$Models
  
  ans$Intercept<-x$Intercept
  
  ans$accuracy<-rbind(x$Accuracy_Train[1:5], x$Accuracy_Test)
  rownames(ans$accuracy)[1]<-"Training Set"
  
  ans<-append(ans, subset(x, !(names(x) %in% c("Method", "Weights", "Intercept", "Accuracy_Train", "Accuracy_Test"))))
  
  class(ans) <- "foreccomb_res_summary"
  return(ans)
}

print.foreccomb_res_summary <- function(x, plot=TRUE) {
  if(class(x)!="foreccomb_res_summary") stop("Data must be class 'foreccomb_res_summary'", call.=FALSE)
  
  cat("\n")
  cat("Summary of Forecast Combination \n")
  cat("------------------------------- \n")
  cat("\n")
  cat("Method: ", x$Method, "\n")
  cat("\n")
  cat("Individual Forecasts & Combination Weights: \n")
  cat("\n")
  print(x$weight)
  cat("\n")
  if (!is.null(x$Intercept)){
    cat("Intercept (Bias-Correction): ", x$Intercept, "\n")
    cat("\n")
  }
  cat("Accuracy of Combined Forecast: \n")
  cat("\n")
  print(x$accuracy)
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
