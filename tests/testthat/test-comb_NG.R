context("comb_NG")

test_that("Forward wrong input to NG", {
  expect_error(comb_NG(1))
  expect_error(comb_NG("abs"))
  expect_error(comb_NG(list(a=1, b=2)))
  expect_error(comb_NG(NULL))
  expect_error(comb_NG(NA))
  expect_error(comb_NG(Inf))
  expect_error(comb_NG(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_NG(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9093589, 1.376927, 1.121035, 126.9625, 328.598, 0.1844116, 0.924408),
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
  result<-comb_NG(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9093589, 1.376927, 1.121035, 126.9625, 328.598, 0.1844116, 0.924408),
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
  result<-comb_NG(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.8482608, 1.233979, 0.9738919, 542.5582, 602.0055),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
