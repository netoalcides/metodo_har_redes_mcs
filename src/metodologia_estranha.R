# metodologia estranha que pensei
forecast_errors <- map( c(1, 5, 10, 15),
                        function(horizons){
                          
                          error_results <- NULL
                          
                          # loop in validation sample
                          foreach( rolling_valid_sample = 1:(T-T_training), .combine='rbind' ) %dopar% {
                            
                            forecasts <- NULL
                            pred <- NULL
                            
                            data_to_loop <- har_original_data_structure %>%
                              slice( (T_training - 21):(T_training + rolling_valid_sample) ) %>%
                              select( date, rv5_252 )
                            
                            # loop recursive forecasts
                            for( h in 1:horizons){
                              
                              new_data <- data_to_loop %>%
                                bind_cols(., har_structure( data_to_loop$rv5_252, har_original_lag_structure ) ) %>%
                                na.omit
                              
                              pred[h] <- predict( fixed_window_har_original_model, new_data %>% last )
                              
                              last_point <- dim(data_to_loop)[1]
                              
                              data_to_loop$rv5_252[last_point] <- pred[h]
                              
                              new_x <- data_frame( date = har_original_data_structure$date[T_training + rolling_valid_sample + h],
                                                   rv5_252 = 0 )
                              
                              data_to_loop %<>%
                                bind_rows(., new_x )
                              
                              forecasts %<>%
                                bind_rows(., data_frame( date = har_original_data_structure$date[T_training + rolling_valid_sample + h - 1],
                                                         original = har_original_data_structure$rv5_252[T_training + rolling_valid_sample + h - 1],
                                                         prediction = pred[h] ) )
                              
                            }
                            
                            error_results <- bind_rows( error_results,
                                                        forecasts %>%
                                                          summarise( start_date = first(date),
                                                                     last_date = last(date),
                                                                     error = mean(original - prediction) ) )
                          }
                        }
)
