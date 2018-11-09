info( logger, "HAR_NEURAL_PROJECT::train and predict classic HAR model - fixed window" )



info( logger, "HAR_NEURAL_PROJECT::train Har classic - fixed window" )
fixed_window_har_original_model <- lm( formula = rv5_252 ~ .,
                                       data =  har_original_data_structure_train %>%
                                         select( -date, -close_price, -log_return, -cdi ) )



info( logger, "HAR_NEURAL_PROJECT::forecasting Har classic - fixed window" )

results_forecasts_har_classic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( round( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {

    # prepare data to forecasts
    data_to_loop <- har_original_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )


    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_har_original_model,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_original_lag_structure,
                               har_dataset =  har_original_data_structure )

    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )

}



info( logger, "HAR_NEURAL_PROJECT::adjust results - fixed window" )

results_forecasts_har_classic_by_horizon.fixed_window <- bind_rows(results_forecasts_har_classic) %>%
  group_by( date, pred_horizon ) %>%
  filter( row_number() <= 1 ) %>%
  ungroup()

cache("results_forecasts_har_classic_by_horizon.fixed_window")

rm( results_forecasts_har_classic )



