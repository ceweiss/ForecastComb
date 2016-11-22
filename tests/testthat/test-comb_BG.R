context("comb_BG")

test_that("Forward wrong input to BG", {
  expect_error(comb_BG(1))
  expect_error(comb_BG("abs"))
  expect_error(comb_BG(list(a=1, b=2)))
  expect_error(comb_BG(NULL))
  expect_error(comb_BG(NA))
  expect_error(comb_BG(Inf))
  expect_error(comb_BG(-Inf))
})

test_that("Check for correct class type and accuracy, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]

  data<-foreccomb(train_o, train_p)
  result<-comb_BG(data)

  expect_is(result, "foreccomb_res")
  expect_length(result, 6)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9966819, 1.427379, 1.183302, 146.3579, 360.6217, 0.1307561, 1.027562),
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
  result<-comb_BG(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(-0.9966819, 1.427379, 1.183302, 146.3579, 360.6217, 0.1307561, 1.027562),
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
  result<-comb_BG(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 8)
  expect_equal(as.vector(result$Accuracy_Test), 
               c(-0.9139565, 1.339539, 1.081617, 612.5169, 646.7535),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})
