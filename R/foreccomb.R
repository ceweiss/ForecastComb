#' @title Format Raw Data for Forecast Combination
#'
#' @description Structures cross-sectional input data (individual model forecasts) for forecast combination. Stores data as S3 class
#' \code{foreccomb} that serves as input to the forecast combination techniques. Handles missing value imputation (optional) and resolves
#' problems due to perfect collinearity.
#'
#' @details
#' The function imports the column names of the prediction matrix (if \code{byrow = FALSE}, otherwise the row names) as model names;
#' if no column names are specified, generic model names are created.
#'
#' The missing value imputation algorithm is a modified version of the EM algorithm for imputation that is applicable to time series
#' data - accounting for correlation between the forecasting models and time structure of the series itself. A smooth spline is
#' fitted to each of the time series at each iteration. The degrees of freedom of each spline are chosen by cross-validation.
#'
#' Forecast combination relies on the lack of perfect collinearity. The test for this condition checks if \code{prediction_matrix} is full
#' rank. In the presence of perfect collinearity, the iterative algorithm identifies the subset of forecasting models that are causing
#' linear dependence and removes the one among them that has the lowest accuracy (according to a selected criterion, default is RMSE).
#' This procedure is repeated until the revised prediction matrix is full rank.
#'
#' @param observed_vector A vector or univariate time series; contains \sQuote{actual values} for training set.
#' @param prediction_matrix A matrix or multivariate time series; contains individual model forecasts for training set.
#' @param newobs A vector or univariate time series; contains \sQuote{actual values} if a test set is used (optional).
#' @param newpreds A matrix or multivariate time series; contains individual model forecasts if a test set is used (optional). Does not
#' require specification of \code{newobs} -- in the case in which a forecaster only wants to train the forecast combination method
#' with a training set and apply it to future individual model forecasts, only \code{newpreds} is required, not \code{newobs}.
#' @param byrow logical. The default (\code{FALSE}) assumes that each column of the forecast matrices (\code{prediction_matrix} and -- if
#' specified -- \code{newpreds}) contains forecasts from one forecast model; if each row of the matrices contains forecasts from
#' one forecast model, set to \code{TRUE}.
#' @param na.impute logical. The default (\code{TRUE}) behavior is to impute missing values via the cross-validated spline approach of
#' the \code{mtsdi} package. If set to \code{FALSE}, forecasts with missing values will be removed. Missing values in the observed data are never
#' imputed.
#' @param criterion One of \code{"RMSE"} (default), \code{"MAE"}, or \code{"MAPE"}. Is only used if \code{prediction_matrix} is not full rank: The algorithm
#' checks which models are causing perfect collinearity and the one with the worst individual accuracy (according to the chosen
#' criterion) is removed.
#'
#' @return Returns an object of class \code{foreccomb}.
#'
#' @examples
#' obs <- rnorm(100)
#' preds <- matrix(rnorm(1000, 1), 100, 10)
#' train_o<-obs[1:80]
#' train_p<-preds[1:80,]
#' test_o<-obs[81:100]
#' test_p<-preds[81:100,]
#'
#' ## Example with a training set only:
#' foreccomb(train_o, train_p)
#'
#' ## Example with a training set and future individual forecasts:
#' foreccomb(train_o, train_p, newpreds=test_p)
#'
#' ## Example with a training set and a full test set:
#' foreccomb(train_o, train_p, test_o, test_p)
#'
#' ## Example with forecast models being stored in rows:
#' preds_row <- matrix(rnorm(1000, 1), 10, 100)
#' train_p_row <- preds_row[,1:80]
#' foreccomb(train_o, train_p_row, byrow = TRUE)
#'
#' ## Example with NA imputation:
#' train_p_na <- train_p
#' train_p_na[2,3] <- NA
#' foreccomb(train_o, train_p_na, na.impute = TRUE)
#'
#' ## Example with perfect collinearity:
#' train_p[,2] <- 0.8*train_p[,1] + 0.4*train_p[,8]
#' foreccomb(train_o, train_p, criterion="RMSE")
#'
#' @author Christoph E. Weiss, Gernot R. Roetzer
#'
#' @seealso
#' \code{\link[mtsdi]{mnimput}},
#' \code{\link[Matrix]{rankMatrix}}
#'
#' @references
#' Junger, W. L., Ponce de Leon, A., and Santos, N. (2003). Missing Data Imputation in Multivariate Time Series
#' via EM Algorithm. \emph{Cadernos do IME}, \bold{15}, 8--21.
#'
#' Dempster, A., Laird, N., and Rubin D. (1977). Maximum Likelihood from Incomplete Data via the EM Algorithm.
#' \emph{Journal of the Royal Statistical Society, Series B}, \bold{39(1)}, 1--38.
#'
#' @keywords manip
#'
#' @importFrom mtsdi mnimput
#' @importFrom stats is.ts
#' @importFrom Matrix rankMatrix
#'
#' @export
foreccomb <- function(observed_vector, prediction_matrix, newobs = NULL, newpreds = NULL, byrow = FALSE, na.impute = TRUE, criterion = "RMSE") {
    if (is.null(observed_vector))
        stop("Training set must contain vector of actual values.", call. = FALSE)
    if (!is.null(dim(observed_vector)) && sum(dim(observed_vector) > 1) > 1)
        stop("The input for 'observed vector' appears to be multidimensional. Training set requires a vector of actual values.", call. = FALSE)
    if (!is.numeric(observed_vector))
        stop("Actual observations (Training Set) are not numeric.", call. = FALSE)
    observed_vector <- as.vector(observed_vector)
    if (is.null(prediction_matrix))
        stop("Training set must contain matrix of individual predictions.", call. = FALSE)
    if (byrow==TRUE){
      prediction_matrix <- t(as.matrix(prediction_matrix))
    } else prediction_matrix <- as.matrix(prediction_matrix)
    if (ncol(prediction_matrix) < 2)
        stop("Forecast Combination requires at least 2 input forecasts.", call. = FALSE)
    if ((length(observed_vector) != nrow(prediction_matrix)))
        stop("Lengths of actual values and prediction matrix do not coincide.", call. = FALSE)
    if (!is.null(newpreds)) {
        if (byrow==TRUE){
          newpreds <- t(as.matrix(newpreds))
        } else newpreds <- as.matrix(newpreds)

        if (ncol(prediction_matrix) != ncol(newpreds))
            stop("Test set predictions and training set predictions must contain same individual forecasts. Number of forecasts differ.", call. = FALSE)
    }

    if (!is.ts(observed_vector)) {
        observed_vector <- stats::as.ts(observed_vector)
    }
    if (sum(is.na(observed_vector)) > 0) {
        stop("Actual observations require non-missingness in training set.", call. = FALSE)
    }

    nmodels <- ncol(prediction_matrix)

    if (!is.ts(prediction_matrix)) {
        prediction_matrix <- stats::as.ts(prediction_matrix)
    }

    ## Check if missing values in training set predictions ---------------------------
    pred_na <- rep(NA, nmodels)
    for (i in 1:nmodels) {
        pred_na[i] <- ifelse(sum(is.na(prediction_matrix[, i])) > 0, FALSE, TRUE)
    }
    if (sum(pred_na) != nmodels) {
        if (na.impute) {
            imputed_values <- imp_values(prediction_matrix, newpreds)
            prediction_matrix <- imputed_values$prediction_matrix
            newpreds <- imputed_values$newpreds
            pred_na <- rep(TRUE, nmodels)
            message("A subset of the individual forecasts included NA values and has been imputed.")
        } else {
            prediction_matrix <- prediction_matrix[, pred_na]
            nmodels <- ncol(prediction_matrix)
            message("A subset of the individual forecasts included NA values and has been removed.")
        }
    }
    if (!is.null(newpreds)) newpreds <- newpreds[, pred_na]

    if (nmodels <= 1L) {
        stop("Forecast combination methods require individual forecasts from at least 2 models.", call. = FALSE)
    }
    if (!is.null(colnames(prediction_matrix))) {
        modelnames <- colnames(prediction_matrix)
    } else {
        modelnames <- rep(NA, nmodels)
        for (i in 1:nmodels) {
            modelnames[i] <- paste0("Model", i)
            message("No model names were provided for prediction matrix. Generic names were generated.")
        }
    }

    ## Check for perfect collinearity in training set predictions --------------------------------
    pred_matrix.rank <- Matrix::rankMatrix(prediction_matrix)[1]
    if (pred_matrix.rank!=nmodels) {
      message("Training set prediction matrix is not full rank. Algorithm to remove linearly dependent models started.")
      revised_preds <- remove_collinear(observed_vector, prediction_matrix, newpreds, criterion)
      prediction_matrix <- stats::as.ts(revised_preds$prediction_matrix)
      if(!is.null(newpreds)) newpreds <- stats::as.ts(revised_preds$newpreds)
      nmodels<-ncol(prediction_matrix)
      modelnames<-colnames(prediction_matrix)
    }

    ## Return Clean Data (can be used for forecast combinations)---------------------------------------
    if (is.null(newobs) & is.null(newpreds)) {
        output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, nmodels = nmodels, modelnames = modelnames), class = c("foreccomb"))
    } else {
        if (is.null(newpreds) == FALSE) {
            colnames(newpreds) <- colnames(prediction_matrix)
            pred_na_test <- rep(NA, nmodels)
            for (i in 1:nmodels) {
                pred_na_test[i] <- ifelse(sum(is.na(newpreds[, i])) > 0, FALSE, TRUE)
            }
            if (sum(pred_na_test) != nmodels) {
                if (na.impute) {
                  imputed_values <- imp_values(prediction_matrix, newpreds)
                  newpreds <- imputed_values$newpreds
                  message("A subset of the individual forecasts included NA values and has been imputed.")
                } else {
                  stop("Prediction matrix (Test Set) must not contain missing values.", call. = FALSE)
                }
            }

            if (is.null(newobs)) {
                output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, Forecasts_Test = newpreds, nmodels = nmodels, modelnames = modelnames),
                  class = c("foreccomb"))
            } else {
                if (!is.numeric(newobs))
                  stop("Actual values (Test Set) are not numeric.", call. = FALSE)
                if (sum(is.na(newobs)) > 0)
                  stop("Actual values (Test Set) must not contain missing values.", call. = FALSE)
                newobs <- as.vector(newobs)
                if (length(newobs) != nrow(newpreds))
                  stop("Lengths of actual values and prediction matrix do not coincide for test set.", call. = FALSE)
                output <- structure(list(Actual_Train = observed_vector, Forecasts_Train = prediction_matrix, Actual_Test = newobs, Forecasts_Test = newpreds, nmodels = nmodels,
                  modelnames = modelnames), class = c("foreccomb"))
            }
        }
    }
    return(output)
}

imp_values <- function(prediction_matrix, newpreds) {
    nobs <- nrow(prediction_matrix)
    whole_data <- rbind(prediction_matrix, newpreds)
    imp_data <- as.matrix(mnimput(~., as.data.frame(whole_data), eps = 0.001, ts = TRUE, method = "spline")$filled.dataset)
    prediction_matrix <- imp_data[1:nobs, ]
    if (!is.null(newpreds)) {
        newpreds <- imp_data[(nobs + 1):nrow(imp_data), ]
    }
    return(list(prediction_matrix = prediction_matrix, newpreds = newpreds))
}

check_rank <- function(prediction_matrix) {
  mat <- prediction_matrix
  pred_matrix.rank <- Matrix::rankMatrix(mat)[1]
  fullrank <- ncol(mat)
  matching <- ifelse(pred_matrix.rank == fullrank, TRUE, FALSE)
  return(matching)
}

remove_collinear <- function(observed_vector, prediction_matrix, newpreds = NULL, criterion="RMSE") {
  mat <- prediction_matrix
  if (!is.null(newpreds)) {
    mat2 <- newpreds
  }

  repeat{
    repeat_this <- check_rank(mat)
    if (repeat_this == TRUE){
      message("The revised matrix is now full rank.")
      break
    }

    ranks<-rep(NA, ncol(mat))
    for (i in 1:ncol(mat)){
      ranks[i] <- Matrix::rankMatrix(mat[,-i])[1]
    }

    maxrank<-which(ranks==max(ranks))
    message("The input matrix is not full rank. The indices of the linearly dependent models are: ", paste(as.character(maxrank), collapse=", "))
    remove_which<-rep(0, ncol(mat))
    for (i in maxrank){
      if (length(grep(criterion, c("MAE", "RMSE", "MAPE"))) == 0) {
        stop("Criterion must be MAE, MAPE, or RMSE.")
      } else {
        if (criterion == "RMSE") {
          remove_which[i] = accuracy(observed_vector, mat[,i])[2]
        } else {
          if (criterion == "MAE") {
            remove_which[i] = accuracy(observed_vector, mat[,i])[3]
          } else remove_which[i] = accuracy(observed_vector, mat[,i])[5]
        }
      }
    }
    remove_this <- which(remove_which == max(remove_which))[1]
    message(paste0("Of these models, model ", remove_this, " had the highest ", criterion, " and was removed."))
    message("Checking if the revised matrix is full rank.")
    mat <- mat[,-remove_this]
    if (!is.null(newpreds)) mat2 <- mat2[,-remove_this]
  }
  revised<-list()
  revised$prediction_matrix<-mat
  if (!is.null(newpreds)){
    revised$newpreds<-mat2
  } else revised$newpreds <- NA
  return(revised)
}
