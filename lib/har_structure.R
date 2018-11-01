# Create har flexible lag structure
# data - realized volatility
# structure - har lag structure: the original proposed by Corsi(2006) is 'structure = c(1, 5, 22)'

har_structure <- function(time_serie_data, structure) {
  
  
  z <- map( structure, function(x){ c( rep( NA, x), lag( rollmean( time_serie_data, x, align = "right"), 1 ) ) } )
  
  z2 <- do.call( cbind, z ) %>% 
    as_data_frame() %>% 
    filter( row_number() != 1 )
  
  names <- paste("rv", structure, sep = "")
  colnames(z2) <- names
  z2
}
