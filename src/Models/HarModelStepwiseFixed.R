info( logger, "HAR_NEURAL_PROJECT::train and predict stepwise HAR model - fixed window" )

info( logger, "HAR_NEURAL_PROJECT::train AIC and BIC models - fixed window" )
# setting up inital models
null <- lm( log(rv5_252) ~ 1, data = har_stepwise_data_structure_train )
full <- lm( log(rv5_252) ~ ., data = har_stepwise_data_structure_train %>%
              select( -date, -close_price, -simple_return, -cdi ) )

# training BIC method
fixed_window_har_stepwise_bic <- step(null, 
                                      scope = list(upper=full), 
                                      direction = "both",  
                                      k = log(T_training), 
                                      trace = 0)

# training AIC method
fixed_window_har_stepwise_aic <- step(null, 
                                      scope = list(upper=full), 
                                      direction = "both",  
                                      k = 2, 
                                      trace = 0)



info( logger, "HAR_NEURAL_PROJECT::forecasting AIC and BIC models - fixed window" )

results_forecasts_har_stepwise_bic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( ceiling( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {

    # prepare data to forecasts
    data_to_loop <- har_stepwise_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )


    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_har_stepwise_bic,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_stepwise_lag_structure,
                               har_dataset =  har_stepwise_data_structure, 
                               model_type = 'log' )

    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )

}

results_forecasts_har_stepwise_aic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( ceiling( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {

    # prepare data to forecasts
    data_to_loop <- har_stepwise_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )


    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_har_stepwise_aic,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_stepwise_lag_structure,
                               har_dataset =  har_stepwise_data_structure, 
                               model_type = 'log' )

    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )

  }



info( logger, "HAR_NEURAL_PROJECT::adjust results - fixed window" )

results_forecasts_har_stepwise_bic_by_horizon.fixed_window <- bind_rows(results_forecasts_har_stepwise_bic) %>%
  group_by( date, pred_horizon ) %>%
  filter( row_number() <= 1 ) %>%
  ungroup()

results_forecasts_har_stepwise_aic_by_horizon.fixed_window <- bind_rows(results_forecasts_har_stepwise_aic) %>%
  group_by( date, pred_horizon ) %>%
  filter( row_number() <= 1 ) %>%
  ungroup()

cache("results_forecasts_har_stepwise_bic_by_horizon.fixed_window")

cache("results_forecasts_har_stepwise_aic_by_horizon.fixed_window")


rm( results_forecasts_har_stepwise_bic, results_forecasts_har_stepwise_aic )



