#' PLACEHOLDER for auto.combine
#'
#' Computes forecast combination weights according to the standard eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The standard eigenvector approach retrieves combination weights from the sample estimated mean squared prediction error matrix
#' as follows:
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param criterion Specifies optimization criterion. Set criterion to either 'RMSE', 'MAE', or 'MAPE'.
#' @param param_list Can contain additional parameters for the different combination methods.
#'
#' @return Returns an object of class 'foreccomb_res'
#' \itemize{
#' \item Method Returns the used forecast combination method.
#' \item Models Returns the individual input models that were used for the forecast combinations.
#' \item Weights Returns the combination weights obtained by applying the combination method to the training set.
#' \item Fitted Returns the fitted values of the combination method for the training set.
#' \item Accuracy_Train Returns range of summary measures of the forecast accuracy for the training set.
#' \item Forecasts_Test Returns forecasts produced by the combination method for the test set. Only returned if input included a forecast matrix for the test set.
#' \item Accuracy_Test Returns range of summary measures of the forecast accuracy for the test set. Only returned if input included a forecast matrix and a vector of actual values for the test set.
#' }
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' data<-foreccomb(train_o, train_p, test_o, test_p)
#' ev_comb_EIG1(data)
#'
#' @seealso
#' \code{\link[GeomComb]{foreccomb}},
#' \code{\link[GeomComb]{plot.foreccomb_res}},
#' \code{\link[GeomComb]{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#'
#' @references
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords ts
#'
#' @import forecast
#'
#' @export
auto.combine <- function(x, criterion, param_list = NULL) {
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

  interm <- ev_comb_EIG1(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- ev_comb_EIG2(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- ev_comb_EIG3(x, ntop_pred = param_list$ntop_pred, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- ev_comb_EIG4(x, ntop_pred = param_list$ntop_pred, criterion = criterion)
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

  interm <- comb_TA(x, trim_factor = param_list$trim_factor, criterion = criterion)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_WA(x, trim_factor = param_list$trim_factor, criterion = criterion)
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

  interm <- comb_QR(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  interm <- comb_CLS(x)
  if (interm$Accuracy_Train[,criterion] < best_so_far$Accuracy_Train[,criterion]) {
    best_so_far <- interm
  }

  return(best_so_far)
}

