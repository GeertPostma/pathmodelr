context("Validity checking of SO-PLS-PM matrix")
library(pathmodelr)

m0 <- 0
m1 <- 1

m0010 <- t(matrix(c(0,0,
                    1,0), nrow=2, ncol=2))

m0020 <- t(matrix(c(0,0,
                    2,0), nrow=2, ncol=2))

mnn1n <- t(matrix(c(-1,-1,
                     1,-1), nrow=2, ncol=2))

m0000 <- t(matrix(c(0,0,
                    0,0), nrow=2, ncol=2))

m0100 <- t(matrix(c(0,1,
                    0,0), nrow=2, ncol=2))

m1111 <- t(matrix(c(1,1,
                    1,1), nrow=2, ncol=2))

m000100110 <- t(matrix(c(0,0,0,
                         1,0,0,
                         1,1,0), nrow=3, ncol=3))

m111111111 <- t(matrix(c(1,1,1,
                         1,1,1,
                         1,1,1), nrow=3, ncol=3))

test_that("accept only matrices larger than 2-by-2", {
  expect_false(is_valid_soplspm_matrix(m0))
  expect_false(is_valid_soplspm_matrix(m1))
  expect_true(is_valid_soplspm_matrix(m0010))

})

test_that("accept only lower triangular matrices", {
  expect_false(is_valid_soplspm_matrix(m0000))
  expect_false(is_valid_soplspm_matrix(m0100))
  expect_false(is_valid_soplspm_matrix(m1111))
  expect_false(is_valid_soplspm_matrix(m111111111))
  expect_true(is_valid_soplspm_matrix(m0010))
  expect_true(is_valid_soplspm_matrix(m000100110))
})

test_that("only accept square matrices", {
  expect_false(is_valid_soplspm_matrix(matrix(1, nrow=3, ncol=2)))
  expect_false(is_valid_soplspm_matrix(matrix(1, nrow=2, ncol=3)))
})

test_that("reject non-binary matrices", {
  expect_false(is_valid_soplspm_matrix(m0020))
  expect_false(is_valid_soplspm_matrix(mnn1n))
})
