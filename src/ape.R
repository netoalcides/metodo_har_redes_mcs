

ape <- function( variable, model, data ){
  
  # seta funcao step
  eps = 1e-7
  setstep <- function(x) {
    x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
  }
  
  
  d0 <- d1 <- data
  
  d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
  d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
  
  d0 %<>% 
    as_data_frame() %>% 
    transmute_at( vars( contains('log_') ), 
                  funs( normalization( x = ., use_sample_parameters = 'no', 
                                       m = sample_mean, s = sample_std_dev) ) )
  
  d1 %<>% 
    as_data_frame() %>% 
    transmute_at( vars( contains('log_') ), 
                  funs( normalization( x = ., use_sample_parameters = 'no', 
                                       m = sample_mean, s = sample_std_dev) ) )
  
  P0 <- predict( model, d0) 
  P1 <- predict( model, d1) 
  
  
  P0 <- denormalize(P0, m = sample_mean, s = sample_std_dev)
  
  P1 <- denormalize(P1, m = sample_mean, s = sample_std_dev)
  
  pe <- (P1 - P0) / (d1[[variable]] - d0[[variable]])
  
  
  ape <- mean(pe)
  
  return(ape)
  
}


dados <- har_original_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  as.data.frame()

variables <- fixed_window_nnhar_original_model$coefnames

for( v in variables){
  
  average_partial_effect <- ape( variable = v, 
       model = fixed_window_nnhar_original_model, 
       data = dados)
  
  show(average_partial_effect)
  
}


