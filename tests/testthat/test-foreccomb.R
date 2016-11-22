context("foreccomb")

test_that("Tests for invalid inputs to foreccomb", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  test_o<-obs[81:100]
  test_p<-preds[81:100,]
  
  expect_error(foreccomb(NULL, train_p))
  expect_error(foreccomb(train_o, NULL))
  
  expect_error(foreccomb(NULL, train_p, test_o, test_p))
  expect_error(foreccomb(train_o, NULL, test_o, test_p))
  expect_error(foreccomb(train_o, train_p, NULL, test_p), NA)
  expect_error(foreccomb(train_o, train_p, test_o, NULL))
})

test_that("Tests for valid training data set", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  testdata<-foreccomb(train_o, train_p)
  all.equal.character(names(testdata),
                      c("Actual_Train", "Forecasts_Train", "nmodels", "modelnames"), 
                      check.attributes = FALSE)
  
  all.equal(length(testdata$Actual_Train), length(train_o))
  all.equal(dim(testdata$Forecasts_Train), dim(train_p))
})

test_that("Tests for valid training and test data sets", {
  set.seed(5)
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  test_o<-obs[81:100]
  test_p<-preds[81:100,]
  
  testdata<-foreccomb(train_o, train_p, test_o, test_p)
  all.equal.character(names(testdata),
                      c("Actual_Train", "Forecasts_Train", "Actual_Test", "Forecasts_Test", "nmodels", "modelnames"), 
                      check.attributes = FALSE)
  
  all.equal(length(testdata$Actual_Train), length(train_o))
  all.equal(length(testdata$Actual_Test), length(test_o))
  all.equal(dim(testdata$Forecasts_Train), dim(train_p))
  all.equal(dim(testdata$Forecasts_Test), dim(test_p))
})