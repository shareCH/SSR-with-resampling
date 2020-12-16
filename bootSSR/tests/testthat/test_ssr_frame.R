context("test ssr frame of bootSSR")

library(testthat)
library(bootSSR)

delta <- .4

n1 <- 50
n2 <- 50

alpha_glob <- .05
alpha_1 <- .03
alpha_0 <- .5

beta <- .2
beta_0 <- .36
beta_tilde <- .64

gamma <- .005/4

b <- 4

B <- 100

set.seed(191111)
control <- rnorm(n1)
treatment <- rnorm(n1, delta)

design <- design(n1, alpha_glob, n2, alpha_0, beta, b)
alpha_1 <- design$alpha_1



test_that("class of object is ssr", {
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = NULL, B,
                     ssrMethod = "ocp", beta_0, beta_tilde, gamma)
  expect_s3_class(actual, "ssr")
})


test_that("ssr frame gives correct value for ocp approach", {
  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "t", B,
                     ssrMethod = "ocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 200, "n1" = 50, "n2" = 150,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .1540, "condPow_true" = .9206,
                   "method" = "ocp")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)


  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "n", B,
                     ssrMethod = "ocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 125, "n1" = 50, "n2" = 75,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0930, "condPow_true" = .6535,
                   "method" = "ocp")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)



  actual <- ssrFrame(control, treatment, design, delta, bootstrap = NULL, B,
                     ssrMethod = "ocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 200, "n1" = 50, "n2" = 150,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .1540, "condPow_true" = .9206,
                   "method" = "ocp")
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


test_that("ssr frame gives correct value for rocp approach", {
  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "t", b,
                     ssrMethod = "rocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 50, "n1" = 50, "n2" = 0,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = 0, "condPow_true" = 0,
                   "method" = "rocp")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)



  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "n", b,
                     ssrMethod = "rocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 88, "n1" = 50, "n2" = 38,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0625, "condPow_true" = .3778,
                   "method" = "rocp")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)



  actual <- ssrFrame(control, treatment, design, delta, bootstrap = NULL, B,
                     ssrMethod = "rocp", beta_0, beta_tilde, gamma)
  expected <- list("n" = 50, "n1" = 50, "n2" = 0,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = 0, "condPow_true" = 0,
                   "method" = "rocp")
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


test_that("ssr frame gives correct value for pz approach", {
  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "t", B,
                     ssrMethod = "pz", beta_0, beta_tilde, gamma)
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



  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "n", B,
                     ssrMethod = "pz", beta_0, beta_tilde, gamma)
  expected <- list("n" = 87, "n1" = 50, "n2" = 37,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0617, "condPow_true" = .3691,
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



  actual <- ssrFrame(control, treatment, design, delta, bootstrap = NULL, B,
                     ssrMethod = "pz", beta_0, beta_tilde, gamma)
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


test_that("ssr frame gives correct value for of approach", {
  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "t", B,
                     ssrMethod = "of", beta_0, beta_tilde, gamma)
  expected <- list("n" = 100, "n1" = 50, "n2" = 50,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0725, "condPow_true" = .4782,
                   "method" = "of")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)


  set.seed(191111)
  actual <- ssrFrame(control, treatment, design, delta, bootstrap = "n", B,
                     ssrMethod = "of", beta_0, beta_tilde, gamma)
  expected <- list("n" = 92, "n1" = 50, "n2" = 42,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0659, "condPow_true" = .4123,
                   "method" = "of")
  expect_equal(actual$n, expected$n)
  expect_equal(actual$n1, expected$n1)
  expect_equal(actual$n2, expected$n2)
  expect_equal(round(actual$delta_obs, 4), expected$delta_obs)
  expect_equal(round(actual$t1, 4), expected$t1)
  expect_equal(actual$delta_ass, expected$delta_ass)
  expect_equal(round(actual$condPow, 4), expected$condPow)
  expect_equal(round(actual$condPow_true, 4), expected$condPow_true)
  expect_equal(actual$method, expected$method)



  actual <- ssrFrame(control, treatment, design, delta, bootstrap = NULL, B,
                     ssrMethod = "of", beta_0, beta_tilde, gamma)
  expected <- list("n" = 100, "n1" = 50, "n2" = 50,
                   "delta_obs" = .1195, "t1" = .5977,
                   "delta_ass" = delta,
                   "condPow" = .0725, "condPow_true" = .4782,
                   "method" = "of")
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
