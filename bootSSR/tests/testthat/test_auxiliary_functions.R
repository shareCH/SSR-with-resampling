context("test auxiliary functions of bootSSR")

library(testthat)
library(bootSSR)

t1 <- 1.3
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
nmax <- design$n1 * design$b

set.seed(191108)
control <- rnorm(n1)
treatment <- rnorm(n1, delta)



### Conditional power

test_that("conditional power is 0 if n = n1", {
  actual <- conditionalPower(t1, n1, n1, alpha_1, w1, w2)
  expected <- 0

  expect_equal(actual, expected)
})


test_that("conditional power is correct if n =! n1", {
  actual <- conditionalPower(t1, 150, n1, alpha_1, w1, w2)
  expected <- .6839

  expect_equal(round(actual, 4), expected)
})


### True conditional power

test_that("true conditional power is 0 if n = n1", {
  actual <- conditionalPower_true(delta, t1, n1, n1, alpha_1, w1, w2)
  expected <- 0

  expect_equal(actual, expected)
})


test_that("true conditional power is correct if n =! n1", {
  actual <- conditionalPower_true(delta, t1, 150, n1, alpha_1, w1, w2)
  expected <- .9290

  expect_equal(round(actual, 4), expected)
})


### Test statistic

test_that("test statistic is correct", {
  actual <- getTestStatistic(1, 2, design)
  expected <- 5
  expect_equal(actual, expected)

  actual <- getTestStatistic(c(1, 1, 1), c(2, 1, 3), design)
  expected <- 5
  expect_equal(actual, expected)

  actual <- getTestStatistic(control, treatment, design)
  expected <- 2.7040
  expect_equal(round(actual, 4), expected)
})


test_that("bootstrapped test statistic is correct", {
  set.seed(191108)
  actual <- boot(B, control, treatment, n1, alpha_0, alpha_1)

  expected <- 1.4076

  expect_equal(round(actual, 4), expected)
})


### Recalculation

test_that("recalculateSampleSize gives correct sample size for ocp", {
  set.seed(191108)
  actual <- recalculateSampleSize(ssrMethod = "ocp", design, t1, w1, w2,
                                  nmax, n_ini, beta_0, beta_tilde, gamma)

  expected <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax)

  expect_equal(actual, expected)
})

test_that("recalculateSampleSize gives correct sample size for rocp", {
  set.seed(191108)
  actual <- recalculateSampleSize(ssrMethod = "rocp", design, t1, w1, w2,
                                  nmax, n_ini, beta_0, beta_tilde, gamma)

  expected <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0)

  expect_equal(actual, expected)
})

test_that("recalculateSampleSize gives correct sample size for pz", {
  set.seed(191108)
  actual <- recalculateSampleSize(ssrMethod = "pz", design, t1, w1, w2,
                                  nmax, n_ini, beta_0, beta_tilde, gamma)

  expected <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini,
                                       beta_tilde)

  expect_equal(actual, expected)
})

test_that("recalculateSampleSize gives correct sample size for of", {
  set.seed(191108)
  actual <- recalculateSampleSize(ssrMethod = "of", design, t1, w1, w2,
                                  nmax, n_ini, beta_0, beta_tilde, gamma)

  expected <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini,
                                        beta_tilde, gamma)

  expect_equal(actual, expected)
})


### Bootstrapped sample size

test_that("boot_n_preparation gives correct sample size", {
  set.seed(191108)
  actual <- boot_n_preparation(control, treatment, design, w1, w2, nmax, n_ini,
                               beta_0, beta_tilde, gamma, ssrMethod = "ocp")

  expected <- 200

  expect_equal(actual, expected)
})

test_that("boot_n gives correct sample size", {
  set.seed(191108)
  actual <- boot_n(ssrMethod = "ocp", B, control, treatment, design, t1, w1, w2,
                   nmax, n_ini, beta_0, beta_tilde, gamma)

  expected <- 75

  expect_equal(actual, expected)
})



### Optimization function

test_that("optimization function gives correct value", {
  actual <- f(150, t1, n1, alpha_1, w1, w2, gamma = .005/4, n_ini)

  expected <- .6214

  expect_equal(round(actual, 4), expected)
})
