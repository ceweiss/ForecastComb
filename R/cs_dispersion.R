cs_dispersion<-function(x, measure=NULL, plot=FALSE){
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  if(!is.null(x$Forecasts_Test)){
    forecast_data <- rbind(x$Forecasts_Train, x$Forecasts_Test)
  } else{
    forecast_data <- x$Forecasts_Train
  }
  
  cs_data<-rep(NA, nrow(forecast_data))
  if (is.null(measure)) stop("Dispersion measure must be 'SD', 'IQR', or 'Range'.", call.=FALSE)
  if(measure=="SD"){
    for (i in 1:nrow(forecast_data)){
      cs_data[i]<-sd(forecast_data[i,])
    }
  }else{
    if(measure=="IQR"){
      for (i in 1:nrow(forecast_data)){
        cs_data[i]<-IQR(forecast_data[i,])
      }
    }else{
      if(measure!="Range") stop("Dispersion measure must be 'SD', 'IQR', or 'Range'.", call.=FALSE)
      
      for (i in 1:nrow(forecast_data)){
        cs_data[i]<-max(forecast_data[i,])-min(forecast_data[i,])
      }
    }
  }
  
  print(cs_data)
  
  if(plot==TRUE){
    pckg <- c("ggplot2")
    temp <- unlist(lapply(pckg, require, character.only=TRUE))
    if (!all(temp==1)) stop("This function requires package \"ggplot2\".\n Use install.packages(\"ggplot2\") if it is not yet installed.\n", call.=FALSE)
    
    pl<-as.data.frame(matrix(NA,ncol=2, nrow=length(cs_data)))
    colnames(pl)<-c("Index", "Value")
    pl[,1]<-1:nrow(pl)
    pl[,2]<-cs_data
    
    p<-ggplot(data=pl, aes(x=Index)) +
      geom_line(aes(y=pl$Value), colour="blue", na.rm=TRUE, size=0.5)+
      scale_x_continuous(breaks = round(seq(0,max(pl$Index),by = nrow(pl)/10),0)) +
      xlab("Index") +
      ylab(paste0(measure)) +
      theme(legend.position = "none") +
      ggtitle("Cross-Sectional Dispersion of Individual Forecasts")+
      theme(plot.title = element_text(size=16, face="bold"))+
      if(!is.null(x$Forecasts_Test)) geom_vline(xintercept = nrow(x$Forecasts_Train), size=1, linetype="longdash", colour="black")
    p
  }
}
