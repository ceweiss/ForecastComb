context("comb_TA")

test_that("Forward wrong input data object", {
  expect_error(comb_TA(1, criterion = "RMSE"))
  expect_error(comb_TA("abs", criterion = "RMSE"))
  expect_error(comb_TA(list(a=1, b=2), criterion = "RMSE"))
  expect_error(comb_TA(NULL, criterion = "RMSE"))
  expect_error(comb_TA(NA, criterion = "RMSE"))
  expect_error(comb_TA(Inf, criterion = "RMSE"))
  expect_error(comb_TA(-Inf, criterion = "RMSE"))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(comb_TA(data, criterion = "RMSE"), NA)
  expect_error(comb_TA(data, criterion = "MAE"), NA)
  expect_error(comb_TA(data, criterion = "MAPE"), NA)
  expect_error(comb_TA(data, criterion = "bb"))
  expect_error(comb_TA(data, criterion = NULL))
  
  expect_error(comb_TA(data, trim_factor = -1))
  expect_error(comb_TA(data, trim_factor = 0.51))
  expect_error(comb_TA(data, trim_factor = 0.25), NA)
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-comb_TA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.002645, 1.434520, 1.190891, 150.254342, 361.450750, 0.125499, 1.034232),
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
  result<-comb_TA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.002645, 1.434520, 1.190891, 150.254342, 361.450750, 0.125499, 1.034232),
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
  result<-comb_TA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.919075, 1.354865, 1.093815, 614.620313, 647.850542),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
