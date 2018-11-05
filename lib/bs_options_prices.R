bs_call <- function( S, K, rf, h, sigma2 ) {
  
  d1 <- ( (log(S/K) + (rf + sigma2/2) * h) / sqrt(sigma2 * h) )
  
  d2 <- d1 - sqrt(sigma2 * h)
  
  (S * pnorm(d1)) - (K * exp(-rf * h) * pnorm(d2))
  
}

bs_put <- function( S, K, rf, h, sigma2 ) {
  
  d1 <- ( (log(S/K) + (rf + sigma2/2) * h) / sqrt(sigma2 * h) )
  
  d2 <- d1 - sqrt(sigma2 * h)
  
  (K * exp(-rf * h) * pnorm(-d2)) - (S * pnorm(-d1))
  
}
