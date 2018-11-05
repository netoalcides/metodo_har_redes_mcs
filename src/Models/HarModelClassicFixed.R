info( logger, "HAR_NEURAL_PROJECT::train and predict classic HAR model - fixed window" )

# training
fixed_window_har_original_model <- lm( formula = rv5_252 ~ .,
                                       data =  har_original_data_structure_train %>%
                                         select( -date, -close_price, -log_return, -cdi ) )

# # forecasting 1, 5, 10, 15 days ahead
# registerDoFuture()
# plan(multiprocess)
# 
# results_forecasts_har_classic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
#   foreach( rolling_valid_sample = 1:( round( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {
# 
#     # prepare data to forecasts
#     data_to_loop <- har_original_data_structure %>%
#       slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
#       select( date, rv5_252 )
# 
# 
#     # loop recursive forecasts
#     forecasts <- har_classic_forecast( model = fixed_window_har_original_model,
#                                        x_reg = data_to_loop,
#                                        horizons = horizons )
# 
# }
# 
# fixed_window.results_forecasts_har_classic_by_horizon <- bind_cols( results_forecasts_har_classic[[1]] %>%
#              rename( prediction_h1 = prediction ),
#            results_forecasts_har_classic[[2]] %>%
#              group_by( date ) %>%
#              filter( row_number() <= 1 ) %>%
#              ungroup() %>%
#              rename( prediction_h5 = prediction ) %>%
#              select(prediction_h5),
#            results_forecasts_har_classic[[3]] %>%
#              group_by( date ) %>%
#              filter( row_number() <= 1 ) %>%
#              ungroup() %>%
#              rename( prediction_h10 = prediction ) %>%
#              select(prediction_h10),
#            results_forecasts_har_classic[[4]] %>%
#              group_by( date ) %>%
#              filter( row_number() <= 1 ) %>%
#              ungroup() %>%
#              rename( prediction_h15 = prediction ) %>%
#              select(prediction_h15) )
# 
# cache("fixed_window.results_forecasts_har_classic_by_horizon")
# 
# rm( results_forecasts_har_classic )


