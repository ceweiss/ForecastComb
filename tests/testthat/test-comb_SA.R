context("comb_SA")

test_that("Forward wrong input to EIG1", {
  expect_error(comb_SA(1))
  expect_error(comb_SA("abs"))
  expect_error(comb_SA(list(a=1, b=2)))
  expect_error(comb_SA(NULL))
  expect_error(comb_SA(NA))
  expect_error(comb_SA(Inf))
  expect_error(comb_SA(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_SA(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
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
  result<-comb_SA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
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
  result<-comb_SA(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.919075, 1.354865, 1.093815, 614.620313, 647.850542),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
