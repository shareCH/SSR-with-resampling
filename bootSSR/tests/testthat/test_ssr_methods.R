context("test ssr methods of bootSSR")

library(testthat)
library(bootSSR)

delta <- .4

n1 <- 50
n2 <- 50
n_ini <- n1 + n2
w1 <- sqrt(n1)
w2 <- sqrt(n2)

alpha_glob <- .05
alpha_1 <- .03
alpha_0 <- .5

beta <- .2
beta_0 <- .36
beta_tilde <- .64

gamma <- .005/4

b <- 4

B <- 100

design <- design(n1, alpha_glob, n2, alpha_0, beta, b)
nmax <- design$b * design$n1


test_that("ocp approach calculates correct n if t1 is not in recalculation area", {
  t1 <- -1
  actual <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax)
  expected <- n1
  expect_equal(actual, expected)

  t1 <- 3
  actual <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax)
  expected <- n1
  expect_equal(actual, expected)
})


test_that("ocp approach calculates correct n if t1 is in recalculation area", {
  t1 <- 1.3
  actual <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax)
  expected <- 193
  expect_equal(actual, expected)

  t1 <- 1
  actual <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax)
  expected <- nmax
  expect_equal(actual, expected)
})


#---------------------------------------------------------------------------


test_that("rocp approach calculates correct n if t1 is not in recalculation area", {
  t1 <- -1
  actual <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)
  expected <- n1
  expect_equal(actual, expected)

  t1 <- 3
  actual <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)
  expected <- n1
  expect_equal(actual, expected)
})


test_that("rocp approach calculates correct n if t1 is in recalculation area", {
  t1 <- 1.3
  actual <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)
  expected <- 193
  expect_equal(actual, expected)

  t1 <- 1.2
  actual <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)
  expected <- nmax
  expect_equal(actual, expected)

  t1 <- 1
  actual <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)
  expected <- n1
  expect_equal(actual, expected)
})


#----------------------------------------------------------------------------


test_that("pz approach calculates correct n if t1 is not in recalculation area", {
  t1 <- -1
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- n1
  expect_equal(actual, expected)

  t1 <- 3
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- n1
  expect_equal(actual, expected)
})


test_that("pz approach calculates correct n if t1 is in recalculation area and in promising zone", {
  t1 <- 1.3
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- 193
  expect_equal(actual, expected)

  t1 <- 1.2
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- nmax
  expect_equal(actual, expected)
})


test_that("pz approach calculates correct n if t1 is in recalculation area but not in promising zone", {
  t1 <- .8
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- n_ini
  expect_equal(actual, expected)

  t1 <- 1.8
  actual <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde)
  expected <- n_ini
  expect_equal(actual, expected)
})


#----------------------------------------------------------------------------


test_that("of approach calculates correct n if t1 is not in recalculation area", {
  t1 <- -1
  actual <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  expected <- n1
  expect_equal(actual, expected)

  t1 <- 3
  actual <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  expected <- n1
  expect_equal(actual, expected)
})


test_that("of approach calculates correct n if t1 is in recalculation area and in promising zone", {
  t1 <- 1.7
  actual <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  expected <- 163
  expect_equal(actual, expected)
})


test_that("of approach calculates correct n if t1 is in recalculation area but not in promising zone", {
  t1 <- .7
  actual <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  expected <- n_ini
  expect_equal(actual, expected)

  t1 <- 1.8
  actual <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  expected <- n_ini
  expect_equal(actual, expected)
})
