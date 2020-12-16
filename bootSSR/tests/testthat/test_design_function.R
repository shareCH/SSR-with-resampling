context("test design function of bootSSR")

library(testthat)
library(bootSSR)

t1 <- 1.3
delta <- .4

n1 <- 50
n2 <- 50

alpha_glob <- .05
alpha_1 <- .03
alpha_0 <- .5

beta <- .2

b <- 4




test_that("design function gives the correct values", {
  actual <- design(n1, alpha_glob, n2, alpha_0, beta, b)
  expected <- list("n1" = n1, "alpha_1" = .03036,
                   "n2" = n2, "alpha_0" = alpha_0,
                   "b" = b, "alpha_glob" = alpha_glob,
                   "beta" = beta)

  expect_equivalent(actual, expected)
})


test_that("design function generates object of class TwoStageDesign", {
  actual <- design(n1, alpha_glob, n2, alpha_0, beta, b)
  expect_s3_class(actual, "TwoStageDesign")
})
