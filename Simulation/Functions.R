get_n <- function(ocp, rocp, pz, gsd = NULL) {
  
  if(!is.null(gsd)){
    n <- matrix(nrow = 4, ncol = length(ocp))
    rownames(n) <- c("OCP", "ROCP", "PZ", "GSD")
    n[1,] <- purrr::map_dbl(ocp, ~ .$n)
    n[2,] <- purrr::map_dbl(rocp, ~ .$n)
    n[3,] <- purrr::map_dbl(pz, ~ .$n)
    n[4,] <- purrr::map_dbl(gsd, ~ .$n)
    n
  } else {
    n <- matrix(nrow = 3, ncol = length(ocp))
    rownames(n) <- c("OCP", "ROCP", "PZ")
    n[1,] <- purrr::map_dbl(ocp, ~ .$n)
    n[2,] <- purrr::map_dbl(rocp, ~ .$n)
    n[3,] <- purrr::map_dbl(pz, ~ .$n)
    n
  }
}


get_cp <- function(ocp, rocp, pz, gsd = NULL) {
  if(!is.null(gsd)){
    cp <- matrix(nrow = 4, ncol = length(ocp))
    rownames(cp) <- c("OCP", "ROCP", "PZ", "GSD")
    cp[1,] <- purrr::map_dbl(ocp, ~ .$condPow)
    cp[2,] <- purrr::map_dbl(rocp, ~ .$condPow)
    cp[3,] <- purrr::map_dbl(pz, ~ .$condPow)
    cp[4,] <- purrr::map_dbl(gsd, ~ .$condPow)
    cp
  } else {
    cp <- matrix(nrow = 3, ncol = length(ocp))
    rownames(cp) <- c("OCP", "ROCP", "PZ")
    cp[1,] <- purrr::map_dbl(ocp, ~ .$condPow)
    cp[2,] <- purrr::map_dbl(rocp, ~ .$condPow)
    cp[3,] <- purrr::map_dbl(pz, ~ .$condPow)
    cp
  }
}



makePlots <- function(boot, boot_n, no_boot, which) {
  purrr::pmap(list(boot, boot_n, no_boot, names(no_boot)),
              ~ ggplot() +
                geom_point(aes(y = ..1[[which]], x = names(..1[[which]]),
                               color = "boot")) +
                geom_point(aes(y = ..2[[which]], x = names(..2[[which]]),
                               color = "boot_n")) +
                geom_point(aes(y = ..3[[which]], x = names(..3[[which]]),
                               color = "noboot")) +
                scale_color_manual(name = "Bootstrapping",
                                   values = c(boot = "#045181", boot_n = "#00B4F0",
                                              noboot = "#84898D"),
                                   labels = c(boot = "test statistic", boot_n = "sample size",
                                              noboot = "without")) +
                scale_x_discrete(limits = c("OCP", "ROCP", "PZ", "GSD"
                )) +
                labs(title = paste("Delta =", ..4, sep = " "),
                     x = "", y = which) +
                theme_minimal() + theme(axis.text.x = element_text(angle = 90))
  )
}

makePlots2 <- function(boot_n, no_boot, which) {
  purrr::pmap(list(boot_n, no_boot, names(no_boot)),
              ~ ggplot() +
                geom_point(aes(y = ..1[[which]], x = names(..1[[which]]),
                               color = "boot_n")) +
                geom_point(aes(y = ..2[[which]], x = names(..2[[which]]),
                               color = "noboot")) +
                scale_color_manual(name = "Bootstrapping",
                                   values = c(boot_n = "#00B4F0",
                                              noboot = "#84898D"),
                                   labels = c(boot_n = "sample size",
                                              noboot = "without")) +
                scale_x_discrete(limits = c("OCP", "ROCP", "PZ", "GSD"
                )) +
                labs(title = paste("Delta =", ..3, sep = " "),
                     x = "", y = which) +
                theme_minimal() + theme(axis.text.x = element_text(angle = 90))
  )
}
