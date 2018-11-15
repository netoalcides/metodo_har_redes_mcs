# Create har flexible lag structure
# data - realized volatility
# structure - har lag structure: the original proposed by Corsi(2006) is 'structure = c(1, 5, 22)'

har_structure <- function(time_serie_data, structure, model_type = c('level', 'sqrt', 'log') ) {
  
  method <- match.arg(model_type)
  
  z <- map( structure, function(x){ c( rep( NA, x), lag( rollmean( time_serie_data, x, align = "right"), 1 ) ) } )
  
  if( method == 'level'){
    
    z2 <- do.call( cbind, z ) %>% 
      as_data_frame() %>% 
      filter( row_number() != 1 )
    
  }
  
  if( method == 'sqrt'){
    
    z2 <- do.call( cbind, z ) %>% 
      as_data_frame() %>% 
      filter( row_number() != 1 ) %>% 
      mutate_all( funs(sqrt) )
    
  }
  
  if( method == 'log'){
    
    z2 <- do.call( cbind, z ) %>% 
      as_data_frame() %>% 
      filter( row_number() != 1 ) %>% 
      mutate_all( funs(log) )
    
  }
  
  names <- paste(method, "_rv", structure, sep = "")
  colnames(z2) <- names
  
  return(z2)
  
}
