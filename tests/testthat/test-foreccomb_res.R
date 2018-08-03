context("foreccomb_res")

test_that("Tests that time series input leads to time series output", {
  obs <- rnorm(100)
  preds <- matrix(rnorm(1000, 1), 100, 10)
  train_o<-obs[1:80]
  train_p<-preds[1:80,]
  
  #train_o and train_p were not cast to ts objects
  train_o <- ts(train_o, start = 2008, frequency = 4)
  train_p <- ts(train_p, start = 2008, frequency = 4)
  fitted <- rnorm(80)
  
  test_o <- ts(obs[81:100], start = 2008, frequency = 4)
  test_p <- ts(preds[81:100,], start = 2008, frequency = 4)
  pred <- rnorm(20)

  fcc_res <-foreccomb_res(method = "test", modelnames = c("test_1", "test_2"), fitted = fitted, pred = pred,
                          accuracy_insample = matrix(c(50, 50), ncol = 2), input_data = list(Actual_Train = train_o,
                          Forecasts_Train = train_p, Actual_Test = test_o, Forecasts_Test = test_p))
  
  expect_true(all(attributes(fcc_res$Fitted)$tsp == attributes(train_o)$tsp))
  expect_true(all(attributes(fcc_res$Forecasts_Test)$tsp == attributes(test_o)$tsp))
  
  expect_true(all(as.vector(fcc_res$Fitted) == fitted))
  expect_true(all(as.vector(fcc_res$Forecasts_Test) == pred))
})
