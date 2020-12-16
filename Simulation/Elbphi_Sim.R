source("Elbphi_function.R")
source("Functions.R")
source("New_score.R")
source("Nrequired.R")

#-------------------------------
# Specify parameters
#-------------------------------

n1 <- 50
alpha_glob <- .025
n2 <- 50
alpha_0 <- .5

delta <- seq(0, 0.5, by = 0.1)

beta <- .2
beta_0 <- .4
beta_tilde <- .64

b <- 4
B <- 5000

design <- bootSSR::design(n1, alpha_glob, n2, alpha_0, beta, b)

nsim <- 10000


# Target values for the score
nreq <- purrr::map(delta, ~ nrequired(., design))
nreq[nreq > design$n1 * design$b] <- design$n1

ref_cp <- as.list(rep(1 - beta, length(delta)))
ref_cp[delta == 0 | nreq == design$n1] <- alpha_glob

#--------------------------------
# Draw test statistics
#--------------------------------


t1 <- purrr::map(delta, function(d) {
  set.seed(200415)
  purrr::map_dfc(1:nsim, ~ rnorm(1, d*sqrt(design$n1/2)))
  })

# Use only recalculation area to compute score
t1_lgl <- purrr::map(t1, function(t1) {
  purrr::map_lgl(t1, function(t1) {
    if(qnorm(1 - design$alpha_0) <= t1 & t1 < qnorm(1 - design$alpha_1)){
      t1 <- TRUE
    } else {
      t1 <- FALSE
    }
    t1
  })
})

t1 <- purrr::map2(t1_lgl, t1, function(t1_lgl, t1) {
  t1[,t1_lgl]
})


#----------------------------------------------
# Recalculation with Elbphi (sample size)
#----------------------------------------------

set.seed(200415)

ocp <- purrr::map(t1, function(t) {
  purrr::map(t, ~ elbphi(., design, B, ssrMethod = "ocp"))
})

rocp <- purrr::map(t1, function(t) {
  purrr::map(t, ~ elbphi(., design, B, ssrMethod = "rocp", beta_0 = beta_0))
  })

pz <- purrr::map(t1, function(t) {
  purrr::map(t, ~ elbphi(., design, B, ssrMethod = "pz", beta_tilde = beta_tilde))
})

gsd <- purrr::map(t1, function(t) {
  purrr::map(t, ~ elbphi(., design, B, ssrMethod = "gsd"))
})



#---------------------------
# Reshape data
#---------------------------

n <- purrr::pmap(list(ocp, rocp, pz, gsd),
                 ~ get_n(..1, ..2, ..3, ..4))

cp <- purrr::pmap(list(ocp, rocp, pz, gsd),
                  ~ get_cp(..1, ..2, ..3, ..4))



#-----------------------------------
# Score
#-----------------------------------

score <- purrr::pmap(list(cp, n, nreq, ref_cp, delta),
                     ~ new_score_vadj(design, ..1, ..4, ..2, ..3, nsim,
                                      plots = FALSE, ..5)
)
names(score) <- delta
