parametric_var <- function( volatility, alpha, type = c("normal", "t_student"), df_ = NULL ) { 
  
  if( type == "normal" ){ var = abs(qnorm( alpha/2, 0, 1) * volatility) }
  
  if( type == "t_student" ){ var = qt( 1 - (alpha/2), df = df_ ) *  volatility }

  return(var)
}
