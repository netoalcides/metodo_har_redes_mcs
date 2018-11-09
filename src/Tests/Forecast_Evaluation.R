

list.files( "./cache", pattern = 'fixed_window.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )



results_forecasts_har_classic_by_horizon.fixed_window

results_forecasts_har_stepwise_bic_by_horizon.fixed_window

results_forecasts_har_stepwise_aic_by_horizon.fixed_window

