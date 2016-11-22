context("comb_EIG2")

test_that("Forward wrong input to EIG2", {
  expect_error(comb_EIG2(1))
  expect_error(comb_EIG2("abs"))
  expect_error(comb_EIG2(list(a=1, b=2)))
  expect_error(comb_EIG2(NULL))
  expect_error(comb_EIG2(NA))
  expect_error(comb_EIG2(Inf))
  expect_error(comb_EIG2(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_EIG2(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(2.196045e-16, 1.033257, 0.8223281, 97.7288, 146.469, 0.1243176, 1.017865),
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
  result<-comb_EIG2(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(2.196045e-16, 1.033257, 0.8223281, 97.7288, 146.469, 0.1243176, 1.017865),
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
  result<-comb_EIG2(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(0.07977107, 1.00866943, 0.83392937, 232.27308969, 240.32910851),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
