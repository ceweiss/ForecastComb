#' @title Result Rbject for the Forecast Combination Methods
#'
#' @description Stores the results and inputs for some combined forecasts.
#'
#' @param method Name of the method. 
#' @param modelnames Names of the models provided by the user. 
#' @param fitted The fitted values.
#' @param accuracy_insample Insample accuracy obtained by the method.
#' @param input_data Contains the input data provided by the user.
#' @param predict (optional) Function used to conduct predictions for new forecasts.
#' @param intercept (optional) Intercept of the method, if it requires one. 
#' @param weights (optional) Weights of the method, if it requires one.
#' @param pred = (optional) Predictions on a test set. 
#' @param accuracy_outsample = (optional) Accuracy on the test set.
#' @param trim_factor = (optional) Trim factor used in some of the methods.
#' @param top_predictors (optional) Number of retained predictors.
#' @param ranking (optional) Ranking of the predictors that determines which models are removed in the trimming step.
#'
#' @return Returns an object of class \code{foreccomb_res}.
#'
#'
#' @author Christoph E. Weiss, Gernot R. Roetzer
#'
#' @importFrom stats is.ts
#'
#' @export
foreccomb_res <- function(method, 
                          modelnames, 
                          fitted,
                          accuracy_insample,
                          input_data,
                          predict = NULL,
                          intercept = NULL, 
                          weights = NULL, 
                          pred = NULL, 
                          accuracy_outsample = NULL,
                          trim_factor = NULL,
                          top_predictors = NULL,
                          ranking = NULL) {

  if(is.ts(input_data$Actual_Train)) {
    fitted <- transfer_ts_prop(input_data$Actual_Train, fitted)
  }
  
  result <- list(Method = method, Models = modelnames, Fitted = fitted, Accuracy_Train = accuracy_insample, 
                 Input_Data = input_data)
  
  if(is.null(rownames(result$Accuracy_Train))) {
    rownames(result$Accuracy_Train) <- "Training Set"
  }
  
  if(!is.null(predict)) {
    result <- append(result, list(Predict = predict))
  }
  
  if(!is.null(intercept)) {
    result <- append(result, list(Intercept = intercept))
  }
  
  if(!is.null(weights)) {
    result <- append(result, list(Weights = weights))
  }
  
  if(!is.null(pred)) {
    if(is.ts(input_data$Actual_Test)) {
      pred <- transfer_ts_prop(input_data$Actual_Test, pred)
    }
    
    result <- append(result, list(Forecasts_Test = pred))
  }
  
  if(!is.null(accuracy_outsample)) {
    result <- append(result, list(Accuracy_Test = accuracy_outsample))
    
    if(is.null(rownames(result$Accuracy_Test))) {
      rownames(result$Accuracy_Test) <- "Test Set"
    }
  }
  
  if(!is.null(trim_factor)) {
    result <- append(result, list(Trim_Factor = trim_factor))
  }
  
  if(!is.null(top_predictors)) {
    result <- append(result, list(Top_Predictors = top_predictors))
  }
  
  if(!is.null(ranking)) {
    result <- append(result, list(Ranking = ranking))
  }
  
  result <- structure(result, class = c("foreccomb_res"))
  
  return(result)
}

transfer_ts_prop <- function(ts, vec) {
  vec <- stats::ts(vec)
  attributes(vec)$tsp <- attributes(ts)$tsp
  return(vec)
}