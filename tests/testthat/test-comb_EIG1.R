context("comb_EIG1")

test_that("Forward wrong input to EIG1", {
  expect_error(comb_EIG1(1))
  expect_error(comb_EIG1("abs"))
  expect_error(comb_EIG1(list(a=1, b=2)))
  expect_error(comb_EIG1(NULL))
  expect_error(comb_EIG1(NA))
  expect_error(comb_EIG1(Inf))
  expect_error(comb_EIG1(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_EIG1(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.006243, 1.439759, 1.196202, 151.7704, 362.437, 0.1223062, 1.040995),
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
  result<-comb_EIG1(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-1.006243, 1.439759, 1.196202, 151.7704, 362.437, 0.1223062, 1.040995),
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
  result<-comb_EIG1(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.921997, 1.36456, 1.100757, 615.9869, 648.1692),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
