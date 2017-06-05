comb_all <- function(x) {
  if (class(x) != "foreccomb")
    stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
  observed_vector <- x$Actual_Train
  prediction_matrix <- x$Forecasts_Train
  modelnames <- x$modelnames
  
  p <- NCOL(prediction_matrix)
  TT <- NROW(prediction_matrix)
  lm0 <- h0 <- list()
  for (i in 1:p) {
    h0[[i]] <- as.matrix(combn(p, i))
  }
  
  tmpfun <- function(xxx, data) {
    # apply annonymos function on each element of the list
    apply(xxx, 2, function(xx, data) {
      lm(observed_vector ~ as.matrix(data)[, xx])
    }, data=data)  # apply this lm on each column of xxx
  }
  
  lm0 <- lapply(h0, tmpfun, data=prediction_matrix)
  
  # Now take the lm objects and compute the information criteria takes lm model as input
  crit_fun <- function(x) {
    k <- length(x$coef)  # will include a constant
    TT <- length(x$res)
    aic <- as.numeric(-2 * logLik(x) + 2 * k)
    aicc <- aic + 2 * (k + 1) * (k + 2)/(TT - k - 1)
    bic <- as.numeric(-2 * logLik(x) + log(TT) * k)
    hq <- as.numeric(-2 * logLik(x) + log(log(TT)) * k)
    return(list(aic = aic, aicc = aicc, bic = bic, hq = hq))
  }
  
  crit_fun_wrap <- function(x) {
    lapply(x, crit_fun)  # apply this function on each element of the list of lm0
  }
  
  # The following line gives list of lists each list is a list of 4 criteria for each lm
  # object (corrsponding to the different combinations)
  tmppcrit <- lapply(lm0, crit_fun_wrap)
  
  # Function to create the X matrix from the different combinations apply annonymos
  # function on each element of the list
  tmpfun <- function(xxx) {
    apply(xxx, 2, function(xx) {
      data.frame(const = rep(1, TT), prediction_matrix[, xx])
    })
  }
  
  tmp_obj <- lapply(h0, tmpfun)
  
  tmpfun <- function(listcoef, listx) {
    t(listcoef$coef %*% t(listx))
  }
  
  tmppcrit2 <- tmpp2 <- list()
  tmpn <- length(lm0)

  for (i in 1:tmpn) {
    tmpp2[[i]] <- mapply(tmpfun, listcoef = lm0[[i]], listx = tmp_obj[[i]])
    tmppcrit2[[i]] <- do.call(cbind, tmppcrit[[i]])
  }
  
  indi_fitted <- do.call(cbind, tmpp2)
  tmppcrit3 <- do.call(cbind, tmppcrit2)
  
  help_fun_w <- function(x) {
    x <- scale(unlist(x), scale = T)  # scaling is necessary to escape zero
    nominatorr <- exp(-0.5 * x)
    nominatorr/sum(nominatorr)
  }
  weigths <- apply(tmppcrit3, 1, help_fun_w)
  
  fitted <- indi_fitted %*% weigths
  # pred <- indi_pred %*% weigths 
  
  accuracy_insample <- apply(fitted, MARGIN = 2, FUN = accuracy, x = observed_vector)
  if (is.null(x$Forecasts_Test) & is.null(x$Actual_Test)) {
    result <- structure(list(Method = "Standard Eigenvector Approach", Models = modelnames, Weights = weights, Fitted = fitted, Accuracy_Train = accuracy_insample,
                             Input_Data = list(Actual_Train = x$Actual_Train, Forecasts_Train = x$Forecasts_Train)), class = c("foreccomb_res"))
#    rownames(result$Accuracy_Train) <- "Training Set"
  }

  return(result)
}