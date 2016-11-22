context("auto_combine")

test_that("Forward wrong input data object", {
  expect_error(auto_combine(1, criterion = "RMSE"))
  expect_error(auto_combine("abs", criterion = "RMSE"))
  expect_error(auto_combine(list(a=1, b=2), criterion = "RMSE"))
  expect_error(auto_combine(NULL, criterion = "RMSE"))
  expect_error(auto_combine(NA, criterion = "RMSE"))
  expect_error(auto_combine(Inf, criterion = "RMSE"))
  expect_error(auto_combine(-Inf, criterion = "RMSE"))
})

test_that("Tests for correct function parameterization", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  expect_error(auto_combine(data, criterion = "RMSE"), NA)
  expect_error(auto_combine(data, criterion = "MAE"), NA)
  expect_error(auto_combine(data, criterion = "MAPE"), NA)
  expect_error(auto_combine(data, criterion = "bb"))
  expect_error(auto_combine(data, criterion = NULL))
})

test_that("Example run of auto_combine", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-auto_combine(data)
  
  expect_is(result, "foreccomb_res")
  expect_length(result, 7)
  expect_equal(as.vector(result$Accuracy_Train), 
               c(0.000000, 0.934598, 0.739543, 97.901652, 108.201288, 0.177286, 0.929727),
               tolerance = 1e-5, 
               check.attributes = FALSE)
})