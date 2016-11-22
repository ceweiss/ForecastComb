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