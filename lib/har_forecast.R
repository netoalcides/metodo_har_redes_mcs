har_forecast <- function(model, x_reg, har_lag_structure, har_dataset, horizons, model_type = c('level', 'sqrt', 'log') ){
  
  forecasts <- NULL
  pred <- NULL
  
  for( h in 1:horizons){
    
    new_data <- x_reg %>%
      bind_cols(., har_structure( x_reg$rv5_252, har_lag_structure, model_type ) ) %>%
      na.omit
    
    pred <- predict( model, new_data %>% last )
    
    pred <- ifelse( model_type == 'sqrt', pred^2, 
            ifelse( model_type == 'log', exp(pred), 
                    pred ) )
    
    last_point <- dim(x_reg)[1]
    
    x_reg$rv5_252[last_point] <- pred
    
    forec <- bind_cols( new_data %>%
                          last %>%
                          select( date, rv5_252 ),
                        data_frame( prediction = pred ) )
    
    forecasts %<>%
      bind_rows(., forec)
    
    new_x <- har_dataset %>%
      filter( date > last(x_reg$date) ) %>%
      slice(1) %>%
      select( date, rv5_252 )
    
    x_reg %<>%
      bind_rows(., new_x )
    
  }
  
  return( forecasts )
}
