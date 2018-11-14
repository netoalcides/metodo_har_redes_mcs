info( logger, "HAR_NEURAL_PROJECT::data preparation for tests" )

list.files( "./cache", pattern = 'fixed_window.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )


# dados
models_predictions <- bind_rows( results_forecasts_har_classic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_classic') ),
                                 results_forecasts_har_stepwise_aic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_aic') ),
                                 results_forecasts_har_stepwise_bic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_bic') )
)

horizons_test <- models_predictions %>% 
  distinct( pred_horizon )

models_test <- models_predictions %>% 
  distinct( model )


