average_partial_effect <- function( variable, model, data, model_type = c('level', 'sqrt', 'log') ){
  
  # set step function
  eps = 1e-7
  setstep <- function(x) {
    x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
  }
  
  # define data
  d0 <- d1 <- data
  
  d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
  d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
  
  # set x + h
  d0 %<>% 
    as_data_frame() %>% 
    transmute_at( vars( contains( model_type ) ), 
                  funs( normalization( x = ., use_sample_parameters = 'no', 
                                       m = sample_mean, s = sample_std_dev) ) )
  
  d1 %<>% 
    as_data_frame() %>% 
    transmute_at( vars( contains(model_type ) ), 
                  funs( normalization( x = ., use_sample_parameters = 'no', 
                                       m = sample_mean, s = sample_std_dev) ) )
  
  # predict data
  P0 <- predict( model, d0) 
  P1 <- predict( model, d1) 
  
  # adjust if data is normalized
  P0 <- denormalize(P0, m = sample_mean, s = sample_std_dev)
  
  P1 <- denormalize(P1, m = sample_mean, s = sample_std_dev)
  
  # partial effect
  pe <- (P1 - P0) / (d1[[variable]] - d0[[variable]])
  
  # average partial effect
  ape <- mean(pe)
  
  return(ape)
  
}

