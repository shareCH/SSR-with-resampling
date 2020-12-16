nrequired <- function(delta, design){

  nreq_out <- c()

  for(i in 1:length(delta)){
    if(delta[i] != 0){
      n_req <- 1 * ((qnorm(1-design$alpha_glob) + qnorm(1-design$beta)) / delta[i])^2
      pow <- power.t.test(n = n_req, delta = delta[i], sd = 1, sig.level = design$alpha_glob,
                          power = NULL,
                          type = c("two.sample"),
                          alternative = "one.sided")$power
      while(pow < 1-design$beta){
        n_req <- n_req + 1
        pow <- power.t.test(n = n_req, delta = delta[i], sd = 1, sig.level = design$alpha_glob,
                            power = NULL,
                            type = c("two.sample"),
                            alternative = "one.sided")$power
      }
      nreq_out[i] <- n_req
    } else{
      nreq_out[i] <- design$n1
    }
  }
  nreq_out
}
