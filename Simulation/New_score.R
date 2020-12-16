new_score_vadj <- function(design, cp, ref_cp, n, nreq, nsim, plots = TRUE, delta = NULL, weight = 1){
  
  #------------------------------------
  # Boxplot and Score for cp
  #------------------------------------
  mean_cp <- rowMeans(cp)
  var_cp <- apply(cp, 1, var)
  if(nrow(cp) == 4){
    rownames(cp) <- c("OCP", "ROCP", "PZ", "GSD")
  } else {
    rownames(cp) <- c("OCP", "ROCP", "PZ")
  }
  
  
  
  if(plots == TRUE) {
    par(mar = c(6.1,5.1,4.1,2.1), cex.axis = 1, cex.lab = 1, mgp = c(3.2,1.2,0))
    boxplot(cp, use.cols = FALSE, col = "lightgray",
            ylab = "Conditional power second stage", main = paste("Delta =", delta))
    abline(h = ref_cp, col = "red", lty = 1)
    points(mean_cp, pch = 4, cex = 2)
  }
  
  
  # location component:
  e_cp <- 1 - abs(mean_cp - ref_cp)/(1 - design$alpha_glob)
  # variation component:
  var_cp_max <- ((1 - 0)/2)^2 * nsim/(nsim - 1)
  v_cp <- 1 - sqrt(var_cp/var_cp_max)
  # total cp subscore:
  if(weight == 1){
    score_cp <- apply(cbind(e_cp, v_cp), 1, mean) 
  } else {
    score_cp <- apply(cbind(e_cp, e_cp, v_cp), 1, mean)
  }
  
  
  #------------------------------------
  # Boxplot and Score for n
  #------------------------------------
  mean_n <- rowMeans(n)
  var_n <- apply(n, 1, var)
  
  if(nrow(n) == 4){
    rownames(n) <- c("OCP", "ROCP", "PZ", "GSD")
  } else{
    rownames(n) <- c("OCP", "ROCP", "PZ")
  }
  
  
  
  if(plots == TRUE) {
    boxplot(n, use.cols = FALSE, col = "lightgray",
            ylab = "Recalculated Sample Size", main = paste("Delta =", delta))
    abline(h = nreq, col = "red", lty = 1)
    points(mean_n, pch = 4, cex = 2)
  }
  
  
  # location component:
  e_n <- 1 - abs(mean_n - nreq)/((design$b - 1) * design$n1)
  # variation component:
  var_n_max <- ((design$n1 * design$b - design$n1)/2)^2 * nsim/(nsim - 1)
  v_n <- 1 - sqrt(var_n/var_n_max)
  # total n subscore:
  if(weight == 1){
    score_n <- apply(cbind(e_n, v_n), 1, mean)
  } else {
    score_n <- apply(cbind(e_n, e_n, v_n), 1, mean)
  }
  
  
  
  
  #------------------------------------
  # Common conditional score
  #------------------------------------
  score_cond <- apply(cbind(score_cp, score_n), 1, mean)
  
  out <- list(
    mean_cp = mean_cp,
    e_cp = e_cp,
    var_cp = var_cp,
    v_cp = v_cp,
    score_cp = score_cp,
    mean_n = mean_n,
    e_n = e_n,
    var_n = var_n,
    v_n = v_n,
    score_n = score_n,
    score_cond = score_cond
  )
  return(out)
}
