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
