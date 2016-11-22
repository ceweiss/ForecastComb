context("comb_LAD")

test_that("Forward wrong input to LAD", {
  expect_error(comb_LAD(1))
  expect_error(comb_LAD("abs"))
  expect_error(comb_LAD(list(a=1, b=2)))
  expect_error(comb_LAD(NULL))
  expect_error(comb_LAD(NA))
  expect_error(comb_LAD(Inf))
  expect_error(comb_LAD(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_LAD(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(0.088853, 0.963358, 0.712742, 87.311788, 98.091194, 0.148402, 0.997116),
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
  result<-comb_LAD(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(0.088853, 0.963358, 0.712742, 87.311788, 98.091194, 0.148402, 0.997116),
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
  result<-comb_LAD(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 9)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(0.142481, 0.881767, 0.672409, 175.377225, 177.413517),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
