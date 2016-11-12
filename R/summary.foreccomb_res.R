#' PLACEHOLDER for summary.foreccomb_res
#'
#' Computes forecast combination weights according to the standard eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The standard eigenvector approach retrieves combination weights from the sample estimated mean squared prediction error matrix 
#' as follows: Suppose \eqn{y_t} is a variable of interest, there are N not perfectly collinear predictors, 
#' \eqn{\bold{f}_t = (f_{1t}, \ldots, f_{Nt})'}, \eqn{\Sigma} is the (positive definite) 
#' mean squared prediction error matrix of \eqn{\bold{f}_t} and \eqn{\bold{e}} is an \eqn{N \times 1}{N * 1} vector of \eqn{(1,\ldots,1)'}. 
#' The N positive eigenvalues are then arranged in increasing order \eqn{(\Phi_1 = \Phi_{min}, \Phi_2, \ldots, \Phi_N)}, and \eqn{\bold{w^j}} 
#' is defined as the eigenvector corresponding to \eqn{\Phi_j}. The combination weights \eqn{\bold{w} = (w_1,\ldots,w_N)'} are then 
#' chosen corresponding to the minimum of \eqn{\left{ \frac{\Phi_1}{d_1^2}, \frac{\Phi_2}{d_2^2},\ldots,\frac{\Phi_N}{d_N^2}\right}}, 
#' denoted as \eqn{\bold{w}^l}, where \eqn{d_j = \bold{e}'\bold{w}^j}, as:
#' \deqn{\bold{w}^{EIG1} = \frac{1}{d_l} \bold{w}^l}
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param object An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param ... Additional parameters
#'
#' @return PLACEHOLDER Returns an object of class 'summary.foreccomb_res'
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
summary.foreccomb_res<-function(object, ...){
  if(class(object)!="foreccomb_res") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  ans <- list()
  
  ans$Method<-object$Method
  
  if(!is.character(object$Weights)) {
    ans$weight<-matrix(object$Weights, ncol=1)
    colnames(ans$weight)<-"Combination Weight"
    rownames(ans$weight)<-object$Models
  } else {
    ans$weight<-"Weights of the individual forecasts differ over time with trimmed mean"
  }
  
  ans$Intercept<-object$Intercept
  
  ans$accuracy<-rbind(object$Accuracy_Train[1:5], object$Accuracy_Test)
  rownames(ans$accuracy)[1]<-"Training Set"
  
  ans$data<-deparse(substitute(object))
  
  ans<-append(ans, subset(object, !(names(object) %in% c("Method", "Weights", "Intercept", "Accuracy_Train", "Accuracy_Test"))))
  
  class(ans) <- c("foreccomb_res_summary")
  
  return(ans)
}

#' PLACEHOLDER for print.foreccomb_res_summary
#'
#' Computes forecast combination weights according to the standard eigenvector approach by Hsiao and Wan (2014) and produces forecasts for the test set, if provided.
#'
#' @details
#' The standard eigenvector approach retrieves combination weights from the sample estimated mean squared prediction error matrix 
#' as follows: Suppose \eqn{y_t} is a variable of interest, there are N not perfectly collinear predictors, 
#' \eqn{\bold{f}_t = (f_{1t}, \ldots, f_{Nt})'}, \eqn{\Sigma} is the (positive definite) 
#' mean squared prediction error matrix of \eqn{\bold{f}_t} and \eqn{\bold{e}} is an \eqn{N \times 1}{N * 1} vector of \eqn{(1,\ldots,1)'}. 
#' The N positive eigenvalues are then arranged in increasing order \eqn{(\Phi_1 = \Phi_{min}, \Phi_2, \ldots, \Phi_N)}, and \eqn{\bold{w^j}} 
#' is defined as the eigenvector corresponding to \eqn{\Phi_j}. The combination weights \eqn{\bold{w} = (w_1,\ldots,w_N)'} are then 
#' chosen corresponding to the minimum of \eqn{\left{ \frac{\Phi_1}{d_1^2}, \frac{\Phi_2}{d_2^2},\ldots,\frac{\Phi_N}{d_N^2}\right}}, 
#' denoted as \eqn{\bold{w}^l}, where \eqn{d_j = \bold{e}'\bold{w}^j}, as:
#' \deqn{\bold{w}^{EIG1} = \frac{1}{d_l} \bold{w}^l}
#' The results are stored in an object of class 'foreccomb_res', for which separate plot and summary functions are provided.
#'
#' @param x An object of class 'foreccomb'. Contains training set (actual values + matrix of model forecasts) and optionally a test set.
#' @param ... Additional parameters
#' 
#' @return Returns formatted output of the foreccomb_res_summary object
#' 
#' @export
print.foreccomb_res_summary <- function(x, ...) {
  if(class(x)!="foreccomb_res_summary") stop("Data must be class 'foreccomb_res_summary'", call.=FALSE)
  
  cat("\n")
  cat("Summary of Forecast Combination \n")
  cat("------------------------------- \n")
  cat("\n")
  cat("Method: ", x$Method, "\n")
  cat("\n")
  cat("Individual Forecasts & Combination Weights: \n")
  if(!is.character(x$weight)) {
    cat("\n")
    print(x$weight)
  } else {
    cat(x$weight)
    cat("\n")
  }
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
  cat("For fitted values (training set): ", paste0(x$data, "$Fitted"), "\n")
  if(!is.null(x$Forecasts_Test)){
    cat("For forecasts (test set): ", paste0(x$data, "$Forecasts_Test"), "\n")
  }
  cat("See ", paste0("str(", x$data, ")"), " for full list.")
}

