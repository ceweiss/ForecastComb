comb_all <- function(x) {
  if (class(x) != "foreccomb")
    stop("Data must be class 'foreccomb'. See ?foreccomb, to bring data in correct format.", call. = FALSE)
  # observed_vector <- x$Actual_Train
  # prediction_matrix <- x$Forecasts_Train
  # modelnames <- x$modelnames
  
  y <- as.vector(x$Actual_Train)
  X <- as.data.frame(x$Forecasts_Train)
  # X is a matrix of predictors
  p <- NCOL(X)
  TT <- NROW(X)
  lm0 <- h0 <- list()
  for (i in 1:p) {
    h0[[i]] <- as.matrix(combn(p, i))
  }
  
  tmpfun <- function(xxx, data) {
    # apply annonymos function on each element of the list
    apply(xxx, 2, function(xx, data) {
      lm(y ~ as.matrix(data)[, xx])
    }, data=data)  # apply this lm on each column of xxx
  }
  
  lm0 <- lapply(h0, tmpfun, data=X)
  
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
      data.frame(const = rep(1, TT), X[, xx])
    })
  }
  
  tmp_obj <- lapply(h0, tmpfun)
  
  tmpfun <- function(listcoef, listx) {
    t(listcoef$coef %*% t(listx))
  }
  
  tmppcrit2 <- tmpp2 <- list()
  tmpn <- length(lm0)
  # pb <- txtProgressBar(min = 1, max = tmpn, style = 3)
  for (i in 1:tmpn) {
    tmpp2[[i]] <- mapply(tmpfun, listcoef = lm0[[i]], listx = tmp_obj[[i]])
    tmppcrit2[[i]] <- do.call(cbind, tmppcrit[[i]])
    # setTxtProgressBar(pb, i)
  }
  
  predd <- do.call(cbind, tmpp2)
  tmppcrit3 <- do.call(cbind, tmppcrit2)
  
  help_fun_w <- function(x) {
    x <- scale(unlist(x), scale = T)  # scaling is necessary to escape zero
    nominatorr <- exp(-0.5 * x)
    nominatorr/sum(nominatorr)
  }
  critw <- apply(tmppcrit3, 1, help_fun_w)
  
  pred0 <- predd %*% critw
  return(list(fit = pred0, weights = critw, indi_pred = predd))
}