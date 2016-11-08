auto.combine <- function(x, criterion, param_list=NULL) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  if(is.null(criterion) || !(criterion %in% c("RMSE", "MAE", "MAPE")))
    stop("Valid optimization criterion is needed. Set criterion to either 'RMSE', 'MAE', or 'MAPE'.", call.=FALSE)
  
  if(!is.null(param_list) && !is.list(param_list)) stop("param_list needs to be a list.")
  
  best_so_far<-ev_comb_EIG1(x)
  
  interm<-ev_comb_EIG2(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-ev_comb_EIG3(x, n_top_predictors=param_list$n_top_predictors, criterion=criterion)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-ev_comb_EIG4(x, n_top_predictors=param_list$n_top_predictors, criterion=criterion)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_InvW(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_SA(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_TA(x, trim_factor=param_list$trim_factor, criterion=criterion)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_WA(x, trim_factor=param_list$trim_factor, criterion=criterion)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_OLS(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_QR(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  interm<-comb_CLS(x)
  if(interm$Accuracy_Train[, criterion] < best_so_far$Accuracy_Train[, criterion]) {
    best_so_far<-interm
  }
  
  return(best_so_far)
}
