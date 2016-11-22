context("comb_InvW")

test_that("Forward wrong input to InvW", {
  expect_error(comb_InvW(1))
  expect_error(comb_InvW("abs"))
  expect_error(comb_InvW(list(a=1, b=2)))
  expect_error(comb_InvW(NULL))
  expect_error(comb_InvW(NA))
  expect_error(comb_InvW(Inf))
  expect_error(comb_InvW(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_InvW(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.963507, 1.407299, 1.143287, 126.799504, 355.253099, 0.110598, 1.037285),
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
  result<-comb_InvW(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.963507, 1.407299, 1.143287, 126.799504, 355.253099, 0.110598, 1.037285),
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
  result<-comb_InvW(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.888633, 1.302919, 1.067756, 595.039633, 632.471188),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
