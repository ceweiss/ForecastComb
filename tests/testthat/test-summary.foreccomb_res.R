context("summary.foreccomb_res")

test_that("Forward wrong input to summary", {
  expect_error(summary.foreccomb_res(1))
  expect_error(summary.foreccomb_res("abs"))
  expect_error(summary.foreccomb_res(list(a=1, b=2)))
  expect_error(summary.foreccomb_res(NULL))
  expect_error(summary.foreccomb_res(NA))
  expect_error(summary.foreccomb_res(Inf))
  expect_error(summary.foreccomb_res(-Inf))
})

test_that("Tests for the correctness of the summary function, when only train set is used", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  data<-foreccomb(train_o, train_p)
  result<-comb_EIG1(data)
  
  sum_object <- summary(result)
  
  expect_s3_class(sum_object, class = "foreccomb_res_summary")
  all.equal.character(names(sum_object), c("Method", "weight", "accuracy", "data", "Models", "Fitted", "Input_Data"))
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
  
  sum_object <- summary(result)
  
  expect_s3_class(sum_object, class = "foreccomb_res_summary")
  all.equal.character(names(sum_object), c("Method", "weight", "accuracy", "data", "Models", "Fitted", "Forecasts_Test", "Input_Data"))
})