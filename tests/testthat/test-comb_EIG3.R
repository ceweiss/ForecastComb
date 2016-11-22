context("comb_EIG3")

test_that("Forward wrong input data object", {
  expect_error(comb_EIG3(1, criterion = "RMSE"))
  expect_error(comb_EIG3("abs", criterion = "RMSE"))
  expect_error(comb_EIG3(list(a=1, b=2), criterion = "RMSE"))
  expect_error(comb_EIG3(NULL, criterion = "RMSE"))
  expect_error(comb_EIG3(NA, criterion = "RMSE"))
  expect_error(comb_EIG3(Inf, criterion = "RMSE"))
  expect_error(comb_EIG3(-Inf, criterion = "RMSE"))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(comb_EIG3(data, criterion = "RMSE"), NA)
  expect_error(comb_EIG3(data, criterion = "MAE"), NA)
  expect_error(comb_EIG3(data, criterion = "MAPE"), NA)
  expect_error(comb_EIG3(data, criterion = "bb"))
  expect_error(comb_EIG3(data, criterion = NULL))
  
  expect_error(comb_EIG3(data, ntop_pred = 0))
  expect_error(comb_EIG3(data, ntop_pred = 1.3))
  expect_error(comb_EIG3(data, ntop_pred = 4), NA)
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_EIG3(data, criterion = "RMSE")

  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9329353, 1.401392, 1.131808, 122.3787, 345.1768, 0.1680234, 1.098073),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})

test_that( "Check for correct class type and accuracy, when Forecast_Test is provided but not Actual_Test", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  test_p<-preds[81:100,]
  
  data<-foreccomb(train_o, train_p, newpreds =  test_p)
  result<-comb_EIG3(data, criterion = "RMSE")
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9329353, 1.401392, 1.131808, 122.3787, 345.1768, 0.1680234, 1.098073),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})

test_that( "Check for correct class type and accuracy, when test set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  test_o<-obs[81:100]
  test_p<-preds[81:100,]
  
  data<-foreccomb(train_o, train_p, test_o, test_p)
  result<-comb_EIG3(data, criterion = "RMSE")
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 10)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.9380776, 1.344189, 1.105871, 670.3206, 721.3069),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
