#' @title Dynamic Forecast Combination
#'
#' @description Computes the dynamic version of the combined forecast for a method included in the GeomComb package.
#'
#' @details
#'
#' The function \code{rolling_combine} allows to estimate a dynamic version of the other combination methods of the package in a
#' standardized way, i.e., it allows for time-varying weights. The function builds on the idea of time series cross-validation:
#' Taking the provided training set as starting point, the models are re-estimated at each period of the test set using a
#' revised (increased) training set.
#'
#' Like univariate dynamic forecasting, the validation approach requires a full test set -- including the observed values.
#'
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Must contrain full training set and test set.
#' @param comb_method The combination method that should be used.
#' @param criterion Specifies loss criterion. Set criterion to either 'RMSE', 'MAE', or 'MAPE' for the methods \code{comb_TA}, \code{comb_WA},
#' \code{comb_EIG3}, and \code{comb_EIG4}, or to 'NULL' (default) for all other methods.
#'
#' @return Returns an object of class \code{foreccomb_res} that represents the results for the best-fit forecast combination method:
#' \item{Method}{Returns the best-fit forecast combination method.}
#' \item{Models}{Returns the individual input models that were used for the forecast combinations.}
#' \item{Weights}{Returns the combination weights obtained by applying the best-fit combination method to the training set.}
#' \item{Fitted}{Returns the fitted values of the combination method for the training set.}
#' \item{Accuracy_Train}{Returns range of summary measures of the forecast accuracy for the training set.}
#' \item{Forecasts_Test}{Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.}
#' \item{Accuracy_Test}{Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.}
#' \item{Input_Data}{Returns the data forwarded to the method.}
#'
#' @author Christoph E. Weiss
#'
#' @references
#' Bergmeir, C., Hyndman, R.J., and Koo, B. (2015). A Note on the Validity of Cross-Validation
#' for Evaluating Time Series Prediction. \emph{Monash University, Deparment of Econometrics and
#' Business Statistics}, Working Paper No. 10/15.
#'
#' Timmermann, A. (2006). Forecast Combinations. \emph{Handbook of Economic Forecasting}, \bold{1}, 135--196.
#'
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#'
#' #Static forecast combination (for example OLS):
#' static_OLS <- comb_OLS(data)
#'
#' #Dynamic forecast combination:
#' dyn_OLS <- rolling_combine(data, "comb_OLS")
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#'
#' @keywords models
#'
#' @import forecast
#'
#' @export


rolling_combine<-function(x, comb_method, criterion=NULL){
  if (class(x) != "foreccomb")
    stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
  observed_vector <- x$Actual_Train
  prediction_matrix <- x$Forecasts_Train
  modelnames <- x$modelnames
  new_obs <- x$Actual_Test
  new_preds <- x$Forecasts_Test

  if(!sum(comb_method==c("comb_BG", "comb_CLS", "comb_CSR", "comb_EIG1", "comb_EIG2", "comb_EIG3", "comb_EIG4",
                        "comb_InvW", "comb_LAD", "comb_MED", "comb_NG", "comb_OLS", "comb_SA", "comb_TA", "comb_WA"))==1)
     stop("Please choose one combination method from the GeomComb package.", call. = FALSE)

  if(comb_method=="comb_CSR")
    stop("Rolling Forecast Combination method is not available for Complete Subset Regression.", call. = FALSE)

  if(is.null(x$Forecasts_Test))
    stop("Rolling Forecast Combination requires input to include a full test set", call. = FALSE)

  meth<-match.fun(comb_method)
  if (is.null(criterion)){
    aux <- meth(x)
  } else{
    aux <- meth(x, criterion=criterion)
  }

  fitted<-aux$Fitted
  accuracy_insample<-aux$Accuracy_Train
  method<-aux$Method
  models<-aux$Models
  weights<-aux$Weights
  roll_forecast<-aux$Forecasts_Test[1]
  intercept<-aux$Intercept

  length_testset<-nrow(new_preds)-1

  for (i in 1:length_testset){
    revised_train.obs<-c(observed_vector, new_obs[1:i])
    revised_train.pred<-rbind(prediction_matrix, new_preds[1:i,])
    if (i<length_testset){
      revised_test.obs<-new_obs[(i+1):(length_testset+1)]
      revised_test.pred<-new_preds[(i+1):(length_testset+1),]
    } else{
      revised_test.obs<-c(new_obs[(i+1):(length_testset+1)], new_obs[(i+1):(length_testset+1)])
      revised_test.pred<-rbind(new_preds[(i+1):(length_testset+1),],new_preds[(i+1):(length_testset+1),])
    }

    revised_data<-foreccomb(revised_train.obs, revised_train.pred, revised_test.obs, revised_test.pred)

    if (is.null(criterion)){
      model <- meth(revised_data)
    } else{
      model <- meth(revised_data, criterion=criterion)
    }

    weights<-rbind(weights, model$Weights)
    if(comb_method=="comb_MED") weights<-"Weights of the individual forecasts differ over time with median method"
    roll_forecast<-c(roll_forecast, model$Forecasts_Test[1])
    if(!is.null(intercept)) intercept<-c(intercept, model$Intercept)
  }

    accuracy_outsample<-accuracy(roll_forecast, new_obs)

    if(!is.null(intercept)){
      result <- structure(list(Method = method, Models = models, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Forecasts_Test = roll_forecast, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                                                                                                          Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
      rownames(result$Accuracy_Train) <- "Training Set"
      rownames(result$Accuracy_Test) <- "Test Set"
    } else{
      result <- structure(list(Method = method, Models = models, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = roll_forecast, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                                                                                                                     Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
      rownames(result$Accuracy_Train) <- "Training Set"
      rownames(result$Accuracy_Test) <- "Test Set"
    }

    return(result)
}


