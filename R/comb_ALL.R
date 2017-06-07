comb_all <- function(x) {
  if (class(x) != "foreccomb")
    stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
  observed_vector <- x$Actual_Train
  prediction_matrix <- x$Forecasts_Train
  modelnames <- x$modelnames
  ic_name_vec <- c("aic", "aicc", "bic",  "hq")
  
  p <- NCOL(prediction_matrix)
  TT <- NROW(prediction_matrix)
  
  ndiff_models <- 2^p - 1
  weights <- matrix(0, ndiff_models, 4)
  indiv_fitted <- matrix(0, length(observed_vector), ndiff_models)
  
  weights_idx <- 1
  indiv_fitted_idx <- 1
  
  conduct_predict <- !is.null(x$Forecasts_Test)
    
  if (conduct_predict) {
    newpred_matrix <- x$Forecasts_Test
    indiv_pred <- matrix(0, NROW(newpred_matrix), ndiff_models)
    indiv_pred_idx <- 1
  }
    
  for (i in 1:p) {
    combinations <- as.matrix(combn(p, i))
    ncombs <- ncol(combinations)
    
    lin_models <- apply(combinations, 2, function(combs, data) {
      lm(observed_vector ~ as.matrix(data)[, combs])
    }, data=prediction_matrix)
    
    end_weights_idx <- weights_idx + ncombs - 1
    weights[weights_idx:end_weights_idx,] <- t(sapply(lin_models, crit_fun))
    weights_idx <- end_weights_idx + 1
    
    end_indiv_fitted_idx <- indiv_fitted_idx + ncombs - 1
    indiv_fitted[, indiv_fitted_idx:end_indiv_fitted_idx] <- sapply(lin_models, FUN = fitted.values)
    indiv_fitted_idx <- end_indiv_fitted_idx + 1
    
    if (conduct_predict) {
      end_indiv_pred_idx <- indiv_pred_idx + ncombs - 1
      indiv_pred[, indiv_pred_idx:end_indiv_pred_idx] <- mapply(comp_predict, lin_models, 1:ncombs, 
                                                                MoreArgs = list(newdata = newpred_matrix, 
                                                                                combinations = combinations))
      indiv_pred_idx <- end_indiv_pred_idx + 1  
    }
  }
  
  weigths <- apply(weights, MARGIN = 2, FUN = comp_normalized_weights)
  colnames(weights) <- ic_name_vec
  
  fitted <- indiv_fitted %*% weigths
  colnames(fitted) <- ic_name_vec

  accuracy_insample <- apply(fitted, MARGIN = 2, FUN = accuracy, x = observed_vector)[1:5,]
  rownames(accuracy_insample) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  colnames(accuracy_insample) <- ic_name_vec
  if(!conduct_predict & is.null(x$Actual_Test)) {
    result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
  }
  
  if(conduct_predict) {
    pred <- indiv_pred %*% weigths
    colnames(pred) <- ic_name_vec
    if(is.null(x$Actual_Test) == TRUE) {
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
    } else {
      newobs_vector <- x$Actual_Test
      accuracy_outsample <- apply(pred, MARGIN = 2, FUN = accuracy, x = newobs_vector)[1:5,]
      rownames(accuracy_outsample) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
      colnames(accuracy_outsample) <- ic_name_vec
      result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                               Forecasts_Test = pred, Accuracy_Test = accuracy_outsample, Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train, Actual_Test = x$Actual_Test,
                                                                                                            Forecasts_Test = x$Forecasts_Test)), class = c("foreccomb_res"))
    }
  }
  
  return(result)
}

# Computes the information criterions for weighting
crit_fun <- function(x) {
  k <- length(x$coef)  # will include a constant
  TT <- length(x$res)
  aic <- as.numeric(-2 * logLik(x) + 2 * k)
  aicc <- aic + 2 * (k + 1) * (k + 2)/(TT - k - 1)
  bic <- as.numeric(-2 * logLik(x) + log(TT) * k)
  hq <- as.numeric(-2 * logLik(x) + log(log(TT)) * k)
  return(c(aic = aic, aicc = aicc, bic = bic, hq = hq))
}

comp_predict <- function(lin_model, comb_idx, newdata, combinations) {
  TT <- NROW(newdata)
  comb <- combinations[,comb_idx]
  pred <- cbind(rep(1, TT), newdata[, comb]) %*% lin_model$coef
  return(pred)
}

comp_normalized_weights <- function(x) {
  x <- scale(unlist(x), scale = T)  # scaling is necessary to escape zero
  nominatorr <- exp(-0.5 * x)
  nominatorr/sum(nominatorr)
}