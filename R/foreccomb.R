#' Create Forecast Combination Input Data
#'
#' Structures cross-sectional input data (individual model forecasts) for forecast combination. Stores data as S3 class 'foreccomb' that is passed on to the geometric forecast combination techniques.
#' 
#' @details
#' The function imports the column names of the prediction matrix (if byrow = FALSE, otherwise the row names) as model names; #
#' if no column names are specified, generic model names are created. 
#'
#' @param observed_vector A vector or univariate time series; contains 'actual values' for training set.
#' @param prediction_matrix A matrix or multivariate time series; contains individual model forecasts for training set.
#' @param newobs A vector or univariate time series; contains 'actual values' if a test set is used.
#' @param newpreds A matrix or multivariate time series; contains individual model forecasts if a test set is used. Does not
#' require specification of 'newobs' -- in the case in which a forecaster only wants to train the forecast combination method
#' with a training set and apply it to future individual model forecasts, only 'newpreds' is required, not 'newobs'.
#' @param byrow logical. The default (FALSE) assumes that each column of the forecast matrices ('prediction_matrix' and -- if
#' specified -- 'newpreds') contains forecasts from one forecast model; if each row of the matrices contains forecasts from
#' one forecast model, set to TRUE.
#' @param na.impute logical. The default (TRUE) behavior is to impute missing values via the cross-validated spline approach of
#' the mtsdi package. If set to FALSE, forecasts with missing values will be removed. Missing values in the observed data are never
#' imputed
#' 
#' @return Returns an object of class 'foreccomb'
#' \itemize{
#'  \item Actual_Train Actual Values (Training Set).
#'  \item Forecasts_Train Retained (non-missing) cross-section of individual model forecasts (Training Set).
#'  \item Actual_Test Actual Values (Test Set). If 'newobs' was provided.
#'  \item Forecasts_Test Retained (non-missing) cross-section of individual model forecasts (Test Set). If 'newpreds' was provided.
#'  \item nmodels Number of retained (non-missing) individual model forecasts.
#'  \item modelnames Model Names of the retained individual forecast models -- either provided or generic names are created (see above).
#' }
#' 
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#' 
#' ## Example with a training set only:
#' foreccomb(train_o, train_p)
#' 
#' ## Example with a training set and future individual forecasts:
#' foreccomb(train_o, train_p, newpreds=test_p)
#' 
#' ## Example with a training set and a full test set:
#' foreccomb(train_o, train_p, test_o, test_p)
#' 
#' ## Example with individual forecast models being stored in rows:
#' preds <- matrix(rnorm(1000, 1), 10, 100)
#' train_p <- preds[,1:80]
#' foreccomb(train_o, train_p, byrow = TRUE)
#' 
#' @seealso
#' ~~objects to See Also as \code{\link{help}}, ~~~
#' 
#' @references 
#'  ~put references to the literature/web site here ~
#' 
#' @keywords ts
#' 
#' @importFrom mtsdi mnimput
#' 
#' @export
foreccomb <- function (observed_vector, prediction_matrix, newobs=NULL, newpreds=NULL, byrow=FALSE, na.impute=TRUE)
{
  if(is.null(observed_vector)) stop("Training set must contain vector of actual values.", call.=FALSE)
  if(!is.null(dim(observed_vector)) && sum(dim(observed_vector)>1)>1) stop("The input for 'observed vector' appears to be multidimensional. Training set requires a vector of actual values.", call.=FALSE)
  if(!is.numeric(observed_vector)) stop("Actual observations (Training Set) are not numeric.", call.=FALSE)
  observed_vector<-as.vector(observed_vector)
  if(is.null(prediction_matrix)) stop("Training set must contain matrix of individual predictions.", call.=FALSE)
  prediction_matrix<-as.matrix(prediction_matrix)
  if(ncol(prediction_matrix)<2) stop("Forecast Combination requires at least 2 input forecasts.", call.=FALSE)
  if(length(observed_vector)!=nrow(prediction_matrix)) stop("Lengths of actual values and prediction matrix do not coincide.", call.=FALSE)
  if (!is.null(newpreds)){
    if(class(newpreds)!="matrix") newpreds<-as.matrix(newpreds)
    
    if (byrow==TRUE){
      if(nrow(prediction_matrix)!=nrow(newpreds)) stop("Test set predictions and training set predictions must contain same individual forecasts. Number of forecasts differ.", call.=FALSE)
    } else{
      if(ncol(prediction_matrix)!=ncol(newpreds)) stop("Test set predictions and training set predictions must contain same individual forecasts. Number of forecasts differ.", call.=FALSE)
    }
  }
  
  if (!is.ts(observed_vector)) {
    observed_vector <- stats::as.ts(observed_vector)
  }
  if (sum(is.na(observed_vector))>0){
    stop("Actual observations require non-missingness in training set.", call.=FALSE)
  }
  
  if (byrow == TRUE){
    predictions<-t(predictions)
  }
  nmodels<-ncol(prediction_matrix)
  if (!is.ts(prediction_matrix)){
    prediction_matrix <- stats::as.ts(prediction_matrix)
  }
  pred_na<-rep(NA, nmodels)
  for (i in 1:nmodels){
    pred_na[i]<-ifelse(sum(is.na(prediction_matrix[,i]))>0, FALSE, TRUE)
  }
  if (sum(pred_na)!=nmodels){
    if(na.impute) {
      imputed_values<-imp.values(prediction_matrix, newpreds)
      prediction_matrix<-imputed_values$prediction_matrix
      newpreds<-imputed_values$newpreds
      pred_na<-rep(TRUE, nmodels)
      message("A subset of the individual forecasts included NA values and has been imputed.")
    } else {
      prediction_matrix<-prediction_matrix[,pred_na]
      nmodels<-ncol(prediction_matrix)
      message("A subset of the individual forecasts included NA values and has been removed.")
    }
  }
  
  if (nmodels <= 1L){
    stop("Forecast combination methods require individual forecasts from at least 2 models.", call.=FALSE)
  }
  if (!is.null(colnames(prediction_matrix))){
    modelnames <- colnames(prediction_matrix)
  } else {
    modelnames<-rep(NA, nmodels)
    for (i in 1:nmodels){
      modelnames[i]<-paste0("Model",i)
      message("No model names were provided for prediction matrix. Generic names were generated.")
    }
  }
  
  if (is.null(newobs) & is.null(newpreds)){
    output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, nmodels = nmodels, modelnames = modelnames),
                        class = c("foreccomb"))
  } else{
    if (is.null(newpreds)==FALSE){
      newpreds <- as.matrix(newpreds)
      if (byrow == TRUE) newpreds <- t(newpreds)
      newpreds <- newpreds[,pred_na]
      colnames(newpreds) <- colnames(prediction_matrix)
      pred_na_test<-rep(NA, nmodels)
      for (i in 1:nmodels){
        pred_na_test[i]<-ifelse(sum(is.na(newpreds[,i]))>0, FALSE, TRUE)
      }
      if (sum(pred_na_test)!=nmodels){
        if(na.impute) {
          imputed_values<-imp.values(prediction_matrix, newpreds)
          newpreds<-imputed_values$newpreds
          message("A subset of the individual forecasts included NA values and has been imputed.")
        } else {
          stop("Prediction matrix (Test Set) must not contain missing values.", call.=FALSE)
        }
      }
      if (is.null(newobs)){
        output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, Forecasts_Test = newpreds, nmodels = nmodels, modelnames = modelnames),
                            class = c("foreccomb"))
      }else{
        if(!is.numeric(newobs)) stop("Actual values (Test Set) are not numeric.", call.=FALSE)
        if(sum(is.na(newobs))>0) stop("Actual values (Test Set) must not contain missing values.", call.=FALSE)
        newobs <- as.vector(newobs)
        if(length(newobs)!=nrow(newpreds)) stop("Lengths of actual values and prediction matrix do not coincide for test set.", call.=FALSE)
        output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, Actual_Test = newobs, Forecasts_Test = newpreds, nmodels = nmodels, modelnames = modelnames),
                            class = c("foreccomb"))
      }
    }
  }
  return(output)
}

imp.values<-function(prediction_matrix, newpreds) {
  nobs<-nrow(prediction_matrix)
  whole_data<-rbind(prediction_matrix, newpreds) 
  imp_data<-as.matrix(mnimput(~.,as.data.frame(whole_data),eps=1e-3,ts=TRUE, method="spline")$filled.dataset)
  prediction_matrix<-imp_data[1:nobs,]
  if(!is.null(newpreds)) {
    newpreds<-imp_data[(nobs+1):nrow(imp_data),]
  }
  return(list(prediction_matrix=prediction_matrix, newpreds=newpreds))
}