#' @title Automated Forecast Combination
#'
#' @description Computes the fit for all the available forecast combination methods on the provided dataset with respect to the loss criterion.
#' Returns the best fit method.
#'
#' @details
#' The function \code{auto_combine} allows to quickly apply all the different forecast combination methods onto the provided time series
#' data and selects the method with the best fit.
#'
#' The user can choose from 3 different loss criteria for the best-fit evaluation:
#' root mean square error (\code{criterion='RMSE'}), mean absolute error (\code{criterion='MAE'}), and
#' mean absolute percentage error (\code{criterion='MAPE'}).
#'
#' In case the user does not want to optimize over the parameters of some of the combination methods,
#' \code{auto_combine} allows to specify the parameter values for these methods explicitly (see Examples).
#'
#' The best-fit results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param criterion Specifies loss criterion. Set criterion to either 'RMSE' (default), 'MAE', or 'MAPE'.
#' @param param_list Can contain additional parameters for the different combination methods (see example below).
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
#' @author Christoph E. Weiss and Gernot R. Roetzer
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
#' # Evaluating all the forecast combination methods and returning the best.
#' # If necessary, it uses the built-in automated parameter optimisation methods
#' # for the different methods.
#' best_combination<-auto_combine(data, criterion = "MAPE")
#'
#' # Same as above, but now we restrict the parameter ntop_pred for the method comb_EIG3 to be 3.
#' param_list<-list()
#' param_list$comb_EIG3$ntop_pred<-3
#' best_combination_restricted<-auto_combine(data, criterion = "MAPE", param_list = param_list)
#'
#' @seealso
#' \code{\link{foreccomb}},
#' \code{\link{plot.foreccomb_res}},
#' \code{\link{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @keywords optimize
#'
#' @import forecast
#'
#' @export
auto_combine <- function(x, criterion="RMSE", param_list = NULL) {
  if (is.null(criterion) || !(criterion %in% c("RMSE", "MAE", "MAPE"))){
    stop("Valid optimization criterion is needed. Set criterion
             to either 'RMSE', 'MAE', or 'MAPE'.", call. = FALSE)
  }

  if (!is.null(param_list) && !is.list(param_list)) {
    stop("param_list needs to be a list.")
  }

  best_so_far <- comb_BG(x)

  interm <- comb_NG(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_EIG1(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_EIG2(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_EIG3(x, ntop_pred = param_list$comb_EIG3$ntop_pred, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_EIG4(x, ntop_pred = param_list$comb_EIG4$ntop_pred, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_InvW(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_SA(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_TA(x, trim_factor = param_list$comb_TA$trim_factor, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_WA(x, trim_factor = param_list$comb_WA$trim_factor, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_MED(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_OLS(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_LAD(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_CLS(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  return(best_so_far)
}

