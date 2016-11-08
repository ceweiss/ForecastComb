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
