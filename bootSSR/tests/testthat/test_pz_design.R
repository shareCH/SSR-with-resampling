context("test pz main function of bootSSR")

library(testthat)
library(bootSSR)

delta <- .4

n1 <- 50
n2 <- 50

alpha_glob <- .05
alpha_1 <- .03
alpha_0 <- .5

beta <- .2
beta_tilde <- .64

b <- 4

B <- 100

set.seed(191111)
control <- rnorm(n1)
treatment <- rnorm(n1, delta)

design <- design(n1, alpha_glob, n2, alpha_0, beta, b)



test_that("class of object is ssr", {
  actual <- pz_design(control, treatment, design, delta,
                      beta_tilde, bootstrap = "t", B)
  expect_s3_class(actual, "ssr")
})


test_that("function calculates correct value for pz approach", {
  set.seed(191111)
  actual <- pz_design(control, treatment, design, delta,
                      beta_tilde, bootstrap = "t", B)
  expected <- list("n" = 100, "n1" = 50, "n2" = 50,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0725, "condPow_true" = .4782,
                   "method" = "pz")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)



  actual <- pz_design(control, treatment, design, delta,
                      beta_tilde, bootstrap = NULL, B)
  expected <- list("n" = 100, "n1" = 50, "n2" = 50,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0725, "condPow_true" = .4782,
                   "method" = "pz")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)
})
