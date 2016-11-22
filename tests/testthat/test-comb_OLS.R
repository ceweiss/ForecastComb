context("comb_OLS")

test_that("Forward wrong input to OLS", {
  expect_error(comb_OLS(1))
  expect_error(comb_OLS("abs"))
  expect_error(comb_OLS(list(a=1, b=2)))
  expect_error(comb_OLS(NULL))
  expect_error(comb_OLS(NA))
  expect_error(comb_OLS(Inf))
  expect_error(comb_OLS(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_OLS(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(3.499805e-17, 0.9345978, 0.7395425, 97.90165, 108.2013, 0.1772856, 0.9297272),
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
  result<-comb_OLS(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(3.499805e-17, 0.9345978, 0.7395425, 97.90165, 108.2013, 0.1772856, 0.9297272),
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
  result<-comb_OLS(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(0.08604833, 0.8460928, 0.6652008, 126.1969, 126.3599),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
