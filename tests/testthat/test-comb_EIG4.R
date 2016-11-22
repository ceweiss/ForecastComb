context("comb_EIG4")

test_that("Forward wrong input data object", {
  expect_error(comb_EIG4(1, criterion = "RMSE"))
  expect_error(comb_EIG4("abs", criterion = "RMSE"))
  expect_error(comb_EIG4(list(a=1, b=2), criterion = "RMSE"))
  expect_error(comb_EIG4(NULL, criterion = "RMSE"))
  expect_error(comb_EIG4(NA, criterion = "RMSE"))
  expect_error(comb_EIG4(Inf, criterion = "RMSE"))
  expect_error(comb_EIG4(-Inf, criterion = "RMSE"))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(comb_EIG4(data, criterion = "RMSE"), NA)
  expect_error(comb_EIG4(data, criterion = "MAE"), NA)
  expect_error(comb_EIG4(data, criterion = "MAPE"), NA)
  expect_error(comb_EIG4(data, criterion = "bb"))
  expect_error(comb_EIG4(data, criterion = NULL))
  
  expect_error(comb_EIG4(data, ntop_pred = 0))
  expect_error(comb_EIG4(data, ntop_pred = 1.3))
  expect_error(comb_EIG4(data, ntop_pred = 4), NA)
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-comb_EIG4(data, criterion = "RMSE")
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-2.633019e-17, 1.027065, 0.814063, 77.23214, 154.8389, 0.1654509, 0.9499373),
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
  result<-comb_EIG4(data, criterion = "RMSE")
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 10)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-2.633019e-17, 1.027065, 0.814063, 77.23214, 154.8389, 0.1654509, 0.9499373),
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
  result<-comb_EIG4(data, criterion = "RMSE")
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 11)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(0.1179975, 0.9556236, 0.7895855, 229.546, 259.2487),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
