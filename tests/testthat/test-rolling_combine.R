context("rolling_combine")

test_that("Forward wrong input to rolling_combine", {
  expect_error(comb_CLS(1))
  expect_error(comb_CLS("abs"))
  expect_error(comb_CLS(list(a=1, b=2)))
  expect_error(comb_CLS(NULL))
  expect_error(comb_CLS(NA))
  expect_error(comb_CLS(Inf))
  expect_error(comb_CLS(-Inf))
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

  test_methods <- c("comb_EIG1", "comb_EIG2", "comb_EIG3", "comb_EIG4", "comb_BG", "comb_OLS",
                    "comb_SA", "comb_LAD")
  correct_example_values <- c(-0.9244591, 1.016278, 1.106519, 241.1729, 642.6906, 0.1086296,
                              1.354865, 0.6951337)
  
  for(i in 1:length(test_methods)) {
    result <- rolling_combine(data, test_methods[i])
    expect_equal(result$Accuracy_Test[((i - 1)%%5) + 1], 
                 correct_example_values[i],
                 tolerance = 1e-5, 
                 check.attributes = FALSE)
  }
})
