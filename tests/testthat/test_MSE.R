context("MSE")
library(pathmodelr)

test_that("Vector MSE", {
  expect_equal(MSE(c(0,0,0,0), c(-2,2,0,0)), 2)
})
