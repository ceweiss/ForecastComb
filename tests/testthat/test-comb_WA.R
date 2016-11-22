context("comb_WA")

test_that("Forward wrong input data object", {
  expect_error(comb_WA(1, criterion = "RMSE"))
  expect_error(comb_WA("abs", criterion = "RMSE"))
  expect_error(comb_WA(list(a=1, b=2), criterion = "RMSE"))
  expect_error(comb_WA(NULL, criterion = "RMSE"))
  expect_error(comb_WA(NA, criterion = "RMSE"))
  expect_error(comb_WA(Inf, criterion = "RMSE"))
  expect_error(comb_WA(-Inf, criterion = "RMSE"))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(comb_WA(data, criterion = "RMSE"), NA)
  expect_error(comb_WA(data, criterion = "MAE"), NA)
  expect_error(comb_WA(data, criterion = "MAPE"), NA)
  expect_error(comb_WA(data, criterion = "bb"))
  expect_error(comb_WA(data, criterion = NULL))
  
  expect_error(comb_WA(data, trim_factor = -1))
  expect_error(comb_WA(data, trim_factor = 0.51))
  expect_error(comb_WA(data, trim_factor = 0.25), NA)
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-comb_WA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.00773, 1.433826, 1.189203, 146.7917, 365.0705, 0.1246514, 1.035604),
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
  result<-comb_WA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.00773, 1.433826, 1.189203, 146.7917, 365.0705, 0.1246514, 1.035604),
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
  result<-comb_WA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.9073744, 1.34317, 1.071896, 619.5004, 652.0929),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
