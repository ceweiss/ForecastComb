context("cs_dispersion")

test_that("Forward wrong input data object", {
  expect_error(cs_dispersion(1))
  expect_error(cs_dispersion("abs"))
  expect_error(cs_dispersion(list(a=1, b=2)))
  expect_error(cs_dispersion(NULL))
  expect_error(cs_dispersion(NA))
  expect_error(cs_dispersion(Inf))
  expect_error(cs_dispersion(-Inf))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(cs_dispersion(data, measure = "SD"), NA)
  expect_error(cs_dispersion(data, measure = "IQR"), NA)
  expect_error(cs_dispersion(data, measure = "Range"), NA)
  expect_error(cs_dispersion(data, measure = "bb"))
  expect_error(cs_dispersion(data, measure = NULL))
})

test_that( "Check for correct class type and accuracy, when Forecast_Test is provided but not Actual_Test", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  test_p<-preds[81:100,]
  
  data<-foreccomb(train_o, train_p, newpreds =  test_p)
  result<-cs_dispersion(data, measure = "SD")
  
  expect_length(result, 2)
  expect_equal(as.vector(result$CS_Dispersion[1:6]), 
               c(1.064505, 0.793982, 1.042518, 0.724194, 0.690324, 0.590318),
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
  result<-cs_dispersion(data, measure = "SD")
  
  expect_length(result, 2)
  expect_equal(as.vector(result$CS_Dispersion[1:6]), 
               c(1.064505, 0.793982, 1.042518, 0.724194, 0.690324, 0.590318),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})