normalization <- function( x, use_sample_parameters = c('no', 'yes'), m = NULL, s = NULL ){ 
  
  if( use_sample_parameters == 'no' ){
    
    if( is.null(m) == TRUE | is.null(s) == TRUE ){ stop( 'please input m and s' ) }
    
    (x - m) / s
    
  } else {
    
    (x - mean(x)) / sd(x)
    
  }
  
}