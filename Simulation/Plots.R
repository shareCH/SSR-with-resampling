library(bootSSR)
library(tidyverse)
library(ggplot2)
library(ggpubr)


source("Elbphi_function.R")
source("Elbphi_function_R2.R")


#-------------------------------
# Specify parameters
#-------------------------------

n1 <- 50
alpha_glob <- .025
n2 <- 50
alpha_0 <- .5

delta <- .4

beta <- .2
beta_0 <- .4
beta_tilde <- .64
gamma <- .005/4

b <- 4
B <- 5000

design <- bootSSR::design(n1, alpha_glob, n2, alpha_0, beta, b)

nsim <- 10000


#-------------------------------
# Simulate data
#-------------------------------

set.seed(191115)
control <- purrr::map_dfc(1:nsim, ~ rnorm(n1))
treatment <- purrr::map_dfc(1:nsim, ~ rnorm(n1, delta))
colnames(control) <- paste("S", 1:nsim, sep = "")
colnames(treatment) <- paste("S", 1:nsim, sep = "")

t1 <- purrr::map2_dbl(control, treatment,
                      ~ getTestStatistic(.x, .y, design))

nreq <- max(min(ceiling(2 * (qnorm(1 - design$alpha_glob) +
                               qnorm(1 - design$beta))^2 / delta^2),
                design$b * design$n1), design$n1)
ref_cp <- 1 - beta
if(delta == 0) {
  nreq <- n1
  ref_cp <- alpha_glob
}



#-------------------------------
# Do the recalculations
#-------------------------------

# without bootstrapping
ocp <- purrr::map2(control, treatment,
                   ~ bootSSR::ocp_design(.x, .y, design, delta,
                                         bootstrap = NULL, B))
rocp <- purrr::map2(control, treatment,
                    ~ bootSSR::rocp_design(.x, .y, design, delta, beta_0,
                                           bootstrap = NULL, B))
pz <- purrr::map2(control, treatment,
                  ~ bootSSR::pz_design(.x, .y, design, delta, beta_tilde,
                                       bootstrap = NULL, B))


# R1
set.seed(200417)
t1_elbphi <- seq(-1, 3, length.out = nsim)
ocp_elbphi <- purrr::map(t1_elbphi, ~ elbphi(., design, B, ssrMethod = "ocp"))
rocp_elbphi <- purrr::map(t1_elbphi, ~ elbphi(., design, B, ssrMethod = "rocp", beta_0 = beta_0))
pz_elbphi <- purrr::map(t1_elbphi, ~ elbphi(., design, B, ssrMethod = "pz", beta_tilde = beta_tilde))


# R2
set.seed(200417)
ocp_elbphi_R2 <- purrr::map(t1_elbphi, ~ elbphi_R2(., design, B, ssrMethod = "ocp"))
rocp_elbphi_R2 <- purrr::map(t1_elbphi, ~ elbphi_R2(., design, B, ssrMethod = "rocp", beta_0 = beta_0))
pz_elbphi_R2 <- purrr::map(t1_elbphi, ~ elbphi_R2(., design, B, ssrMethod = "pz", beta_tilde = beta_tilde))


#----------------------------
# Reshape data
#----------------------------

n_ocp <- matrix(nrow = nsim, ncol = 3)
colnames(n_ocp) <- c("OCP", "OCP elbphi", "OCP elbphi R2")

n_ocp[,1] <- purrr::map_dbl(ocp, ~ .$n)
n_ocp[,2] <- purrr::map_dbl(ocp_elbphi, ~ .$n)
n_ocp[,3] <- purrr::map_dbl(ocp_elbphi_R2, ~ .$n)

n_rocp <- matrix(nrow = nsim, ncol = 3)
colnames(n_rocp) <- c("ROCP", "ROCP elbphi", "ROCP elbphi R2")

n_rocp[,1] <- purrr::map_dbl(rocp, ~ .$n)
n_rocp[,2] <- purrr::map_dbl(rocp_elbphi, ~ .$n)
n_rocp[,3] <- purrr::map_dbl(rocp_elbphi_R2, ~ .$n)

n_pz <- matrix(nrow = nsim, ncol = 3)
colnames(n_pz) <- c("PZ", "PZ elbphi", "PZ elbphi R2")

n_pz[,1] <- purrr::map_dbl(pz, ~ .$n)
n_pz[,2] <- purrr::map_dbl(pz_elbphi, ~ .$n)
n_pz[,3] <- purrr::map_dbl(pz_elbphi_R2, ~ .$n)


#----------------------
# Plots
#----------------------

ggplot() + geom_line(aes(x = t1, y = n_ocp[,1], color = "noboot"))  +
  geom_line(aes(x = t1_elbphi, y = n_ocp[,2], color = "elbphi")) +
  geom_line(aes(x = t1_elbphi, y = n_ocp[,3], color = "elbphi_R2")) +
  scale_color_manual("", values = c(noboot = "#84898D",
                                    elbphi = "#045181",
                                    elbphi_R2 = "springgreen4"
  ),
  labels = c(noboot = "original",
             elbphi = "R1 approach",
             elbphi_R2 = "R2 approach"
  )
  ) +
  theme_minimal() +
  ylim(0, 225) +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20, colour = "black"),
        legend.position = c(.8, .8),
        legend.text = element_text(size = 20, colour = "black"),
        legend.title = element_text(size = 20, colour = "black")) +
  labs(title = expression(bold(atop(" ", paste("Observed conditional power approach")))), 
       y = "Total sample size", x = "Observed interim test statistic") +
  theme(plot.title = element_text(hjust = 0)) +
  xlim(-.5, 3.5)


ggplot() + geom_line(aes(x = t1, y = n_rocp[,1], color = "noboot")) +
  geom_line(aes(x = t1_elbphi, y = n_rocp[,2], color = "elbphi")) +
  geom_line(aes(x = t1_elbphi, y = n_rocp[,3], color = "elbphi_R2")) +
  scale_color_manual("", values = c(noboot = "#84898D",
                                    elbphi = "#045181",
                                    elbphi_R2 = "springgreen4"
  ),
  labels = c(noboot = "original",
             elbphi = "R1 approach",
             elbphi_R2 = "R2 approach"
  )
  ) +
  ylim(0, 225) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20, colour = "black"),
        legend.position = c(.8, .8),
        legend.text = element_text(size = 20, colour = "black"),
        legend.title = element_text(size = 20, colour = "black")) +
  labs(title = expression(bold(atop(" ", paste("Restricted observed conditional power approach")))), 
       y = "Total sample size", x = "Observed interim test statistic") +
  theme(plot.title = element_text(hjust = 0)) +
  xlim(-.5, 3.5)


ggplot() + geom_line(aes(x = t1, y = n_pz[,1], color = "noboot")) +
  geom_line(aes(x = t1_elbphi, y = n_pz[,2], color = "elbphi")) +
  geom_line(aes(x = t1_elbphi, y = n_pz[,3], color = "elbphi_R2")) +
  scale_color_manual("", values = c(noboot = "#84898D",
                                    elbphi = "#045181",
                                    elbphi_R2 = "springgreen4"
  ),
  labels = c(noboot = "original",
             elbphi = "R1 approach",
             elbphi_R2 = "R2 approach"
  )
  ) +
  ylim(0, 225) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20, colour = "black"),
        legend.position = c(.8, .8),
        legend.text = element_text(size = 20, colour = "black"),
        legend.title = element_text(size = 20, colour = "black")) +
  labs(title = expression(bold(atop(" ", paste("Promising zone approach")))), 
       y = "Total sample size", x = "Observed interim test statistic") +
  theme(plot.title = element_text(hjust = 0)) +
  xlim(-.5, 3.5)