context("comb_ALL")

test_that("Forward wrong input to comb_ALL", {
  expect_error(comb_ALL(1))
  expect_error(comb_ALL("abs"))
  expect_error(comb_ALL(list(a=1, b=2)))
  expect_error(comb_ALL(NULL))
  expect_error(comb_ALL(NA))
  expect_error(comb_ALL(Inf))
  expect_error(comb_ALL(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-comb_ALL(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
  expect_equal(diag(result$Accuracy_Train), 
               c(5.762535e-18, 0.9398780, 0.7488595, 99.05076),
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
  result<-comb_ALL(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(diag(result$Accuracy_Train), 
               c(5.762535e-18, 0.9398780, 0.7488595, 99.05076),
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
  result<-comb_ALL(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(diag(result$Accuracy_Test), 
               c(0.1072318, 0.8706946, 0.6841234, 114.2225440),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})