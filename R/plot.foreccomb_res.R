#' PLACEHOLDER for plot.foreccomb_res
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
#'
#' @return Returns an object of class 'foreccomb_res'
#' \itemize{
#' \item x bla
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
#' \code{\link[GeomComb2]{foreccomb}},
#' \code{\link[GeomComb2]{plot.foreccomb_res}},
#' \code{\link[GeomComb2]{summary.foreccomb_res}},
#' \code{\link[forecast]{accuracy}}
#' 
#' @references 
#' Hsiao, C., and Wan, S. K. (2014). Is There An Optimal Forecast Combination? \emph{Journal of Econometrics}, \bold{178(2)}, 294--309.
#'
#' @keywords ts
#' 
#' @import ggplot2
#' 
#' @export
plot.foreccomb_res<-function(x, ...) {
  if(class(x)!="foreccomb_res") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  method<-x$Method
  fit<-x$Fitted
  forec<-x$Forecasts_Test
  observed_vector<-x$Input_Data$Actual_Train
  newobs_vector<-x$Input_Data$Actual_Test
  
  if (is.null(forec) & is.null(newobs_vector)){
    cols <- c("ACTUAL"="black","COMBINED (FIT)"="#F04546")
    
    pl<-as.data.frame(matrix(NA,ncol=3, nrow=length(observed_vector)))
    colnames(pl)<-c("Index", "Actual", "Combined_Fit")
    pl[,1]<-1:nrow(pl)
    pl[,2]<-c(observed_vector)
    pl[,3]<-fit
    
    p<-ggplot(data=pl, aes(x=Index)) +
      geom_line(aes(y=pl$Actual, colour="ACTUAL"), size=0.5) +
      geom_line(aes(y=pl$Combined_Fit, colour="COMBINED (FIT)"), size=0.8) +
      scale_x_continuous(breaks = round(seq(0,max(pl$Index),by = nrow(pl)/10),0)) +
      scale_colour_manual(name="Series", values=cols) +
      guides(colour = guide_legend(override.aes = list(size=c(0.5,0.8)))) +
      xlab("Index") +
      ylab(paste0(method,"\n Fitted Values/Forescasts")) +
      ggtitle(paste0(method," Forecast Combination \n Actual vs. Fitted/Test Set Forecasts")) +
      theme(plot.title = element_text(size=16, face="bold"))+
      theme(legend.title = element_text(colour="black", size=12, face="bold"))
    p
  } else{
    
    cols <- c("ACTUAL" = "black", "COMBINED (FIT)" = "#F04546", "COMBINED (FORECAST)" = "#F04546")
    
    if(is.null(newobs_vector)==FALSE){
      pl<-as.data.frame(matrix(NA,ncol=4, nrow=(length(observed_vector)+length(newobs_vector))))
      colnames(pl)<-c("Index", "Actual", "Combined_Fit", "Combined_Forecast")
      pl[,1]<-1:nrow(pl)
      pl[,2]<-c(observed_vector,newobs_vector)
      pl[,3]<-c(fit, rep(NA,length(forec)))
      pl[,4]<-c(rep(NA,length(fit)),forec)
      pl[length(observed_vector),4]<-pl[length(observed_vector),3]
      
      p<-ggplot(data=pl, aes(x=Index)) +
        geom_line(aes(y=pl$Actual, colour="ACTUAL"), size=0.5) +
        geom_line(aes(y=c(pl$Combined_Fit), colour="COMBINED (FIT)"),na.rm=TRUE, size=0.8) +
        geom_line(aes(y=c(pl$Combined_Forecast), colour="COMBINED (FORECAST)"),na.rm=TRUE, size=1.5) +
        scale_x_continuous(breaks = round(seq(0,max(pl$Index),by = nrow(pl)/10),0)) +
        scale_colour_manual(name="Series", values=cols) +
        guides(colour = guide_legend(override.aes = list(size=c(0.5,0.8,1.5)))) +
        xlab("Index") +
        ylab(paste0(method, "\n Fitted Values/Forecasts")) +
        ggtitle(paste0(method," Forecast Combination \n Actual vs. Fitted/Test Set Forecast")) +
        theme(plot.title = element_text(size=16, face="bold"))+
        theme(legend.title = element_text(colour="black", size=12, face="bold"))+
        geom_vline(xintercept = length(observed_vector), size=1, linetype="longdash", colour="black")
      p
    }
    else{
      pl<-as.data.frame(matrix(NA,ncol=4, nrow=(length(observed_vector)+length(forec))))
      colnames(pl)<-c("Index", "Actual", "Combined_Fit", "Combined_Forecast")
      pl[,1]<-1:nrow(pl)
      pl[,2]<-c(observed_vector,rep(NA,length(forec)))
      pl[,3]<-c(fit, rep(NA,length(forec)))
      pl[,4]<-c(rep(NA,length(fit)),forec)
      pl[length(observed_vector),4]<-pl[length(observed_vector),3]
      
      p<-ggplot(data=pl, aes(x=Index)) +
        geom_line(aes(y=pl$Actual, colour="ACTUAL"), na.rm=TRUE, size=0.5) +
        geom_line(aes(y=c(pl$Combined_Fit), colour="COMBINED (FIT)"),na.rm=TRUE, size=0.8) +
        geom_line(aes(y=c(pl$Combined_Forecast), colour="COMBINED (FORECAST)"),na.rm=TRUE, size=1.5) +
        scale_x_continuous(breaks = round(seq(0,max(pl$Index),by = nrow(pl)/10),0)) +
        scale_colour_manual(name="Series", values=cols) +
        guides(colour = guide_legend(override.aes = list(size=c(0.5,0.8,1.5)))) +
        xlab("Index") +
        ylab(paste0(method, "\n Fitted Values")) +
        ggtitle(paste0(method," Forecast Combination \n Actual vs. Fitted")) +
        theme(plot.title = element_text(size=16, face="bold"))+
        theme(legend.title = element_text(colour="black", size=12, face="bold"))+
        geom_vline(xintercept = length(observed_vector), size=1, linetype="longdash", colour="black")
      p
    }
  }
}
