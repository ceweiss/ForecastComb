#Convenience plot function with several options - ggplot:
plot.foreccomb_res<-function(x) {
  pckg <- c("ggplot2")
  temp <- unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"ggplot2\".\n Use install.packages(\"ggplot2\") if it is not yet installed.\n", call.=FALSE)
  
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
