sign_hit <- function( pred, real ){
  
  mean( ifelse( real * pred > 0, 1, 0) )
  
}
