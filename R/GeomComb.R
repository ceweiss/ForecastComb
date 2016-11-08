#GeomComb package:

foreccomb <- function (observed_vector, prediction_matrix, newobs=NULL, newpreds=NULL, byrow=FALSE) 
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
    message("A subset of the individual forecasts included NA values and has been removed.")
  }
  prediction_matrix<-prediction_matrix[,pred_na]
  nmodels<-ncol(prediction_matrix)

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
        stop("Prediction matrix (Test Set) must not contain missing values.", call.=FALSE)
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

#Geometric Forecast Combination Methods:

#Standard Eigenvector Approach:

ev_comb_EIG1 <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  error_matrix <- observed_vector - prediction_matrix
  sample_msqu_pred_error <- (t(error_matrix)%*%error_matrix)/length(observed_vector)
  eigen_decomp <- eigen(sample_msqu_pred_error)
  ds <- colSums(eigen_decomp$vectors)
  adj_eigen_vals <- eigen_decomp$values/(ds^2)
  min_idx <- which.min(adj_eigen_vals)
  
  weights<-eigen_decomp$vectors[, min_idx]/ds[min_idx]
  fitted <- as.vector(prediction_matrix%*%weights)
  accuracy_insample <- accuracy(fitted,observed_vector)
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                       class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, 
                          Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, 
                               Forecasts_Test = pred, Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}


#Bias-Corrected Eigenvector Approach:

ev_comb_EIG2 <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  mean_obs <- mean(observed_vector)
  mean_preds <- colMeans(prediction_matrix)
  centered_obs <- observed_vector - mean_obs
  centered_preds <- scale(prediction_matrix, scale=FALSE)
  omega_matrix <- t(centered_obs - centered_preds)%*%(centered_obs - centered_preds)/length(observed_vector) 
  eigen_decomp <- eigen(omega_matrix)
  ds <- colSums(eigen_decomp$vectors)
  adj_eigen_vals <- eigen_decomp$values/(ds^2)
  min_idx <- which.min(adj_eigen_vals)

  weights <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
  intercept <- as.numeric(mean_obs - t(mean_preds)%*%weights)
  fitted <- as.vector(as.vector(intercept)+prediction_matrix%*%weights)
  accuracy_insample<-accuracy(fitted,observed_vector)
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Weights = weights, Intercept = intercept, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(as.vector(intercept)+newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, 
                               Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = intercept, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample, 
                               Forecasts_Test = pred, Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }

  return(result)
}

#Trimmed Eigenvector Approach - helper function for computations:
comp.EIG3 <- function(observed_vector, prediction_matrix, n_top_predictors) {
  error_matrix <- observed_vector - prediction_matrix
  sum_sq_error <- colSums((error_matrix)^2)
  ranking <- rank(sum_sq_error)
  filter_vec <- ranking <= n_top_predictors
  adj_error_matrix <- error_matrix[, filter_vec]
  sample_msqu_pred_error <- (t(adj_error_matrix)%*%adj_error_matrix)/length(observed_vector)
  eigen_decomp <- eigen(sample_msqu_pred_error)
  ds <- colSums(eigen_decomp$vectors)
  adj_eigen_vals <- eigen_decomp$values/(ds^2)
  min_idx <- which.min(adj_eigen_vals)
  
  weights <- numeric(length(ranking))
  weights[filter_vec] <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
  fitted <- as.vector(prediction_matrix%*%weights)
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  return(list(weights = weights, fitted = fitted, accuracy_insample = accuracy_insample, ranking=ranking))
}

#Trimmed Eigenvector Approach - main function:
ev_comb_EIG3 <- function(x, n_top_predictors=NULL, criterion = NULL) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  if(!is.null(n_top_predictors)) {
    if (!(n_top_predictors>=1) || !((n_top_predictors%%1)==0) || !(n_top_predictors<=ncol(prediction_matrix))) { 
      stop("Trimmed eigenvector combination requires either a positive integer between 
         [1, number of predictors], specifying the number of predictors to retain, or 
         a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').", call.=FALSE)    
    }
    
    eig3_res<-comp.EIG3(observed_vector, prediction_matrix, n_top_predictors)
    
    weights<-eig3_res$weights
    fitted<-eig3_res$fitted
    accuracy_insample<-eig3_res$accuracy_insample
    ranking<-eig3_res$ranking

  } else if(!is.null(criterion) && (criterion %in% c("RMSE", "MAE", "MAPE"))) {
    iter<-ncol(prediction_matrix)
    
    n_top_predictors<-1
    interm<-comp.EIG3(observed_vector, prediction_matrix, n_top_predictors)
    best_so_far<-interm
    
    for(i in 2:iter) {
      interm<-comp.EIG3(observed_vector, prediction_matrix, n_top_predictors=i)
      if(interm$accuracy_insample[, criterion] < best_so_far$accuracy_insample[, criterion]) {
        best_so_far<-interm
        n_top_predictors<-i
      }
      
      weights<-best_so_far$weights
      fitted<-best_so_far$fitted
      accuracy_insample<-best_so_far$accuracy_insample
      ranking<-best_so_far$ranking
    }    
  } else
    stop("Trimmed eigenvector combination requires either a positive integer between 
         [1, number of predictors], specifying the number of predictors to retain, or 
         a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').", call.=FALSE)

  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Trimmed Eigenvector Approach", Models = modelnames, Weights = weights, Top_Predictors=n_top_predictors, 
                             Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Trimmed Eigenvector Approach", Models = modelnames, Weights = weights, Top_Predictors=n_top_predictors, 
                               Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Trimmed Eigenvector Approach", Models = modelnames, Weights = weights, Top_Predictors=n_top_predictors, 
                               Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Trimmed Mean-Corrected Eigenvector Approach:
comp.EIG4 <- function(observed_vector, prediction_matrix, n_top_predictors) {
  sum_sq_error <- colSums((observed_vector - prediction_matrix)^2)
  ranking <- rank(sum_sq_error)
  filter_vec <- ranking <= n_top_predictors
  adj_prediction_matrix <- as.matrix(prediction_matrix[, filter_vec])
  mean_obs <- mean(observed_vector)
  mean_preds <- colMeans(adj_prediction_matrix)
  centered_obs <- observed_vector - mean_obs
  centered_preds <- scale(adj_prediction_matrix, scale=FALSE)
  omega_matrix <- t(centered_obs - centered_preds)%*%(centered_obs - centered_preds)/length(observed_vector) 
  eigen_decomp <- eigen(omega_matrix)
  ds <- colSums(eigen_decomp$vectors)
  adj_eigen_vals <- eigen_decomp$values/(ds^2)
  min_idx <- which.min(adj_eigen_vals)
  
  weights <- numeric(length(ranking))
  weights[filter_vec] <- eigen_decomp$vectors[, min_idx]/ds[min_idx]
  intercept <- mean_obs - t(mean_preds)%*%weights[filter_vec]
  ranking <- ranking
  fitted <- as.vector(as.vector(intercept)+prediction_matrix%*%weights)
  accuracy_insample<-accuracy(fitted,observed_vector)
  
  return(list(intercept = intercept, weights = weights, fitted = fitted, accuracy_insample = accuracy_insample, ranking = ranking))
}

ev_comb_EIG4 <- function(x, n_top_predictors=NULL, criterion=NULL) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  if(!is.null(n_top_predictors)) {
    if (!(n_top_predictors>=1) || !((n_top_predictors%%1)==0) || !(n_top_predictors<=ncol(prediction_matrix))) { 
      stop("Trimmed eigenvector combination requires either a positive integer between 
           [1, number of predictors], specifying the number of predictors to retain, or 
           a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').", call.=FALSE)    
    }
    
    eig4_res <- comp.EIG4(observed_vector, prediction_matrix, n_top_predictors)
    
    intercept<-eig4_res$intercept
    weights<-eig4_res$weights
    fitted<-eig4_res$fitted
    accuracy_insample<-eig4_res$accuracy_insample
    ranking<-eig4_res$ranking
    
    } else if(!is.null(criterion) && (criterion %in% c("RMSE", "MAE", "MAPE"))) {
      iter<-ncol(prediction_matrix)
      
      n_top_predictors<-1
      interm<-comp.EIG4(observed_vector, prediction_matrix, n_top_predictors)
      best_so_far<-interm
      
      for(i in 2:iter) {
        interm<-comp.EIG4(observed_vector, prediction_matrix, n_top_predictors=i)
        if(interm$accuracy_insample[, criterion] < best_so_far$accuracy_insample[, criterion]) {
          best_so_far<-interm
          n_top_predictors<-i
        }
        
        intercept<-best_so_far$intercept
        weights<-best_so_far$weights
        fitted<-best_so_far$fitted
        accuracy_insample<-best_so_far$accuracy_insample
        ranking<-best_so_far$ranking
      }    
    } else
      stop("Trimmed eigenvector combination requires either a positive integer between 
         [1, number of predictors], specifying the number of predictors to retain, or 
         a valid optimization criterion ('RMSE', 'MAE', or 'MAPE').", call.=FALSE)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights, Top_Predictors=n_top_predictors, 
                             Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(as.vector(intercept)+newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights, Top_Predictors=n_top_predictors, 
                               Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Trimmed Bias-Corrected Eigenvector Approach", Models = modelnames, Intercept = as.numeric(intercept), Weights = weights, Top_Predictors=n_top_predictors, 
                               Ranking = unname(ranking), Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Other non-regression based combination approaches:

#Simple Average:

comb_SA <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  weights <- rep(1/ncol(prediction_matrix), ncol(prediction_matrix))
  fitted <- as.vector(prediction_matrix%*%weights)
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Simple Average", Models = modelnames, Weights = weights, 
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }

  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Simple Average", Models = modelnames, Weights = weights,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Simple Average", Models = modelnames, Weights = weights, 
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Trimmed Mean (Including option for optimization of trim factor based on training set):

comb_TA <- function(x, trim_factor=NULL, criterion=NULL) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires package \"forecast\".\n Use install.packages(\"forecast\") if it is not yet installed.\n", call.=FALSE)
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  if(!is.null(trim_factor)){
    if(!is.numeric(trim_factor)) stop("Trimming Factor must be numeric.", call.=FALSE)
    if(abs(trim_factor)>0.5) stop("Trimming Factor must be between 0 and 0.5.", call.=FALSE)
    trimf<-trim_factor
    adj_pred<-apply(prediction_matrix,1, function(x) mean(x, trim=trimf, na.rm=TRUE))
  }
  else{
    if (is.null(criterion)) stop("Automatic optimization of trim factor requires selection of 'criterion'.", call.=FALSE)
    if (length(grep(criterion, c("MAE", "MAPE", "RMSE")))!=1) stop("Criterion for trim factor optimization must be 'MAE', 'MAPE', or 'RMSE'.", call.=FALSE)
    aux_matrix<-matrix(NA, nrow=51, ncol=1)
    rownames(aux_matrix)<-seq(0,0.5,0.01)
    for (i in 1:51){
      if (criterion=="RMSE") aux_matrix[i,]<-accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim=((i-1)/100), na.rm=TRUE)), observed_vector)[2]
      if (criterion=="MAE") aux_matrix[i,]<-accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim=((i-1)/100), na.rm=TRUE)), observed_vector)[3]
      if (criterion=="MAPE") aux_matrix[i,]<-accuracy(apply(prediction_matrix, 1, function(x) mean(x, trim=((i-1)/100), na.rm=TRUE)), observed_vector)[5]
    }
    best<-which(aux_matrix==min(aux_matrix))[1]
    trimf<-as.numeric(rownames(aux_matrix)[best])
    
    adj_pred<-apply(prediction_matrix,1, function(x) mean(x, trim=trimf, na.rm=TRUE))
  }
  
  weights <- "Weights of the individual forecasts differ over time with trimmed mean"
  fitted <- adj_pred
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights, 
                             Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- apply(newpred_matrix, 1, function(x) mean(x, trim=trimf))
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights,
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Trimmed Mean", Models = modelnames, Weights = weights, 
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Winsorized Mean (including option for optimization of trim factor based on training set):
comb_WA <- function(x, trim_factor=NULL, criterion=NULL) {
  pckg<-c("forecast", "psych")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"psych\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  if(!is.null(trim_factor)){
    if(!is.numeric(trim_factor)) stop("Trimming Factor must be numeric.", call.=FALSE)
    if(abs(trim_factor)>0.5) stop("Trimming Factor must be between 0 and 0.5.", call.=FALSE)
    trimf<-trim_factor
    adj_pred<-apply(prediction_matrix,1, function(x) winsor.mean(x, trim=trimf, na.rm=TRUE))
  }
  else{
    if (is.null(criterion)) stop("Automatic optimization of trim factor requires selection of 'criterion'.", call.=FALSE)
    if (length(grep(criterion, c("MAE", "MAPE", "RMSE")))!=1) stop("Criterion for trim factor optimization must be 'MAE', 'MAPE', or 'RMSE'.", call.=FALSE)
    aux_matrix<-matrix(NA, nrow=51, ncol=1)
    rownames(aux_matrix)<-seq(0,0.5,0.01)
    for (i in 1:51){
      aux_matrix[i,]<-accuracy(apply(prediction_matrix, 1, function(x) winsor.mean(x, trim=((i-1)/100),na.rm=TRUE)), observed_vector)[2]
    }
    best<-which(aux_matrix==min(aux_matrix))[1]
    trimf<-as.numeric(rownames(aux_matrix)[best])
    
    adj_pred<-apply(prediction_matrix,1, function(x) winsor.mean(x, trim=trimf, na.rm=TRUE))
  }
  
  weights <- "Weights of the individual forecasts differ over time with winsorized mean"
  fitted <- adj_pred
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights, 
                             Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- apply(newpred_matrix, 1, function(x) winsor.mean(x, trim=trimf))
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights,
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Winsorized Mean", Models = modelnames, Weights = weights, 
                               Trimming_Factor = trimf, Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Inverse Ranking Approach:
comb_InvW <- function(x) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  error_matrix <- observed_vector - prediction_matrix
  sum_sq_error <- colSums((error_matrix)^2)
  ranking <- rank(sum_sq_error)
  inv_ranking <- 1/ranking
  
  weights <- inv_ranking/sum(inv_ranking)
  fitted <- as.vector(prediction_matrix%*%weights)
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights, 
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    pred <- as.vector(newpred_matrix%*%weights)
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Inverse Ranking Approach", Models = modelnames, Weights = weights, 
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Regression-Based Approaches:

#Ordinary Least Squares:
comb_OLS <- function(x) {
  pckg<-c("forecast", "ForecastCombinations")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"ForecastCombinations\".\n Use install.packages() if they are not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  regression<-Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "ols")
  
  weights <- regression$weights[2:length(regression$weights)]
  intercept <- regression$weights[1]
  fitted <- as.vector(regression$fitted[,1])
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Ordinary Least Squares Regression", Models = modelnames, Weights = weights, Intercept = intercept,
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    regression_aux<-Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "ols")
    pred <- as.vector(regression_aux$pred[,1])
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Ordinary Least Squares Regression", Models = modelnames, Weights = weights, Intercept = intercept,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Ordinary Least Squares Regression", Models = modelnames, Weights = weights, Intercept = intercept, 
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Robust Regression:
comb_QR <- function(x) {
  pckg<-c("forecast", "ForecastCombinations")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"ForecastCombinations\".\n Use install.packages() if they are not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  regression<-Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "robust")
  
  weights <- regression$weights[2:length(regression$weights)]
  intercept <- regression$weights[1]
  fitted <- as.vector(regression$fitted[,1])
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept,
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    regression_aux<-Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "robust")
    pred <- as.vector(regression_aux$pred[,1])
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Robust Regression (QR)", Models = modelnames, Weights = weights, Intercept = intercept, 
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

#Constrained Least Squares:
comb_CLS <- function(x) {
  pckg<-c("forecast", "ForecastCombinations")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\" and \"ForecastCombinations\".\n Use install.packages() if they are not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  observed_vector<-x$Actual_Train
  prediction_matrix<-x$Forecasts_Train
  modelnames <- x$modelnames
  
  regression<-Forecast_comb(observed_vector, prediction_matrix, Averaging_scheme = "cls")
  
  weights <- regression$weights[2:length(regression$weights)]
  fitted <- as.vector(regression$fitted[,1])
  accuracy_insample <- accuracy(fitted,observed_vector)
  
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)){
    result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights,
                             Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), 
                        class = c("foreccomb_res"))
    rownames(result$Accuracy_Train)<-"Training Set"
  }
  
  if (is.null(x$Forecasts_Test)==FALSE){
    newpred_matrix<-x$Forecasts_Test
    regression_aux<-Forecast_comb(observed_vector, prediction_matrix, fhat_new = newpred_matrix, Averaging_scheme = "cls")
    pred <- as.vector(regression_aux$pred[,1])
    if (is.null(x$Actual_Test)==TRUE){
      result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights,
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
    }else{
      newobs_vector<-x$Actual_Test
      accuracy_outsample <- accuracy(pred, newobs_vector)
      result <- structure(list(Method = "Constrained Least Squares Regression", Models = modelnames, Weights = weights, 
                               Fitted = fitted, Accuracy_Train = accuracy_insample, Forecasts_Test = pred,
                               Accuracy_Test = accuracy_outsample,
                               Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test, Forecasts_Test = x$Forecasts_Test)), 
                          class = c("foreccomb_res"))
      rownames(result$Accuracy_Train)<-"Training Set"
      rownames(result$Accuracy_Test)<-"Test Set"
    }
  }
  return(result)
}

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

summary.foreccomb_res<-function(x, plot=TRUE){
  if(class(x)!="foreccomb_res") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
      
    cat("\n")
    cat("Summary of Forecast Combination \n")
    cat("------------------------------- \n")
    cat("\n")
    Method<-x$Method
    cat("Method: ", Method, "\n")
    cat("\n")
    cat("Individual Forecasts & Combination Weights: \n")
    cat("\n")
    weight<-matrix(x$Weights, ncol=1)
    colnames(weight)<-"Combination Weight"
    rownames(weight)<-x$Models
    print(weight)
    cat("\n")
    if (!is.null(x$Intercept)){
      cat("Intercept (Bias-Correction): ", x$Intercept, "\n")
      cat("\n")
    }
    cat("Accuracy of Combined Forecast: \n")
    cat("\n")
    accuracy<-rbind(x$Accuracy_Train[1:5], x$Accuracy_Test)
    rownames(accuracy)[1]<-"Training Set"
    print(accuracy)
    cat("\n")
    cat("Additional information can be extracted from the combination object: \n")
    cat("For fitted values (training set): ", paste0(deparse(substitute(x)), "$Fitted"), "\n")
    if(!is.null(x$Forecasts_Test)){
     cat("For forecasts (test set): ", paste0(deparse(substitute(x)), "$Forecasts_Test"), "\n")
    }
    cat("See ", paste0("str(", deparse(substitute(x)), ")"), " for full list.")
    
    if(plot==TRUE){
      plot(x)
    }
}

auto.combine <- function(x, criterion, param_list=NULL) {
  pckg<-c("forecast")
  temp<-unlist(lapply(pckg, require, character.only=TRUE))
  if (!all(temp==1)) stop("This function requires packages \"forecast\".\n Use install.packages() if it is not yet installed.\n", call.=FALSE)  
  
  if(class(x)!="foreccomb") stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call.=FALSE)
  
  # if(is.null(observed_vector)) stop("Training set must contain vector of actual values.", call.=FALSE)
  # if(!is.numeric(observed_vector)) stop("Argument 'observed_vector' is not numeric.", call.=FALSE)
  # if(is.null(prediction_matrix)) stop("Training set must contain prediction matrix.", call.=FALSE)
  # if(class(prediction_matrix)!="matrix") prediction_matrix<-as.matrix(prediction_matrix)
  # if(ncol(prediction_matrix)<2) stop("Forecast Combination requires at least 2 input forecasts.", call.=FALSE)
  # if(length(observed_vector)!=nrow(prediction_matrix)) stop("Lengths of actual values and prediction matrix do not coincide.", call.=FALSE)
  # 
  # if(is.null(newpred_matrix)==FALSE){
  #   if(class(newpred_matrix)!="matrix") newpred_matrix<-as.matrix(newpred_matrix)
  #   if(ncol(newpred_matrix)!=ncol(prediction_matrix)) stop("Test set prediction matrix must contain same number of columns as training set prediction matrix.", call.=FALSE)
  # }
  
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

#Tests:
set.seed(1)

obs <- rnorm(100)
preds <- matrix(rnorm(1000, 1), 100, 10)
train_o<-obs[1:80]
train_p<-preds[1:80,]
test_o<-obs[81:100]
test_p<-preds[81:100,]

test_data<-foreccomb(train_o, train_p, test_o, test_p)

test_res1 <- ev_comb_EIG1(test_data)
test_res2 <- ev_comb_EIG2(test_data)
test_res3 <- ev_comb_EIG3(test_data, n_top_predictors = 4)
test_res4 <- ev_comb_EIG4(test_data, criterion = "MAE")
test_res5 <- comb_SA(test_data)
test_res6 <- comb_TA(test_data, criterion="MAE")
test_res7 <- comb_WA(test_data, criterion="MAE")
test_res8 <- comb_InvW(test_data)

test_res9 <- auto.combine(test_data, criterion = "RMSE")
test_res10 <- auto.combine(test_data, criterion = "MAE")

plot(test_res3)
plot(test_res7)

comp.EIG3(test_data$Actual_Train, test_data$Forecasts_Train, 3)

#install.packages("ForecastCombinations")
library(ForecastCombinations)
test_simple<-Forecast_comb(train_o, train_p, Av="simple")
test_ols<-Forecast_comb(train_o, train_p, Av="ols")
test_robust<-Forecast_comb(train_o, train_p, Av="robust")
test_cls<-Forecast_comb(train_o, train_p, Av="cls")
test_varb<-Forecast_comb(train_o, train_p, Av="variance based")
test_best<-Forecast_comb(train_o, train_p, Av="best")

#The functions are meant to enhance the available forecast combination methods in R
#The package 'ForecastCombinations' mostly includes regression-based approaches (OLS, Robust Linear Regression, CLS)
#Is not very sophisticated, no S3 classes introduced, so not suitable for convenience functions and
#Regression-Based Methods in the Package create errors due to collinearity issues

#Still left to do:
###################################
# - Write a convenience function for summary() and print() for this object class - CW
# - Introduce a S3 object class that is then passed on to plot(), summary(), and print() - CW
# - Write a function for "unexperienced" users that simply provides forecasts based on the input data,
#   just like the function forecast() in the forecast package does. This selection algorithm can be a simple
#   grid-search, or Bayesian - GR
# - Make sure package upload will work without errors - GR
# - Hsiao (2011) found that regression-based methods tend to have relative advantage when one or more
#   models are better than the others, while eigenvector-based methods tend to have relative advantage when
#   individual model forecasts are in the same ball park. Suggests that introducing a 'dynamic model selection'
#   algorithm similar to the 'Switching Rule' in the inflation paper, but based on the criterion of 
#   cross-sectional dispersion among the individual forecasts, is an interesting contribution to the 
#   forecast combination methodology - CW (can be left for later version)
# - Possibly add additional non-regression based approaches (but can be left for future versions), aim is to get this
#   published asap on CRAN to avoid the risk that a new version of ForecastCombinations package might be published that
#   includes similar functions, therefore getting the documentation ready is important - both (can be left for later version)
##################################
