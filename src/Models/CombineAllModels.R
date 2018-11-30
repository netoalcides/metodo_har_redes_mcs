info( logger, "HAR_NEURAL_PROJECT::data preparation for tests" )

list.files( "./cache", pattern = 'fixed_window.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )

# dados
models_predictions <- bind_rows( results_forecasts_har_classic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_classic') ),
                                 results_forecasts_har_stepwise_aic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_aic') ),
                                 results_forecasts_har_stepwise_bic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_bic') ),
                                 results_forecasts_nnhar_classic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('nnhar_classic') ),
                                 results_forecasts_nnhar_stepwise_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('nnhar_stepwise') ),
                                 results_forecasts_bnnhar_classic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('bnnhar_classic') ),
                                 results_forecasts_bnnhar_stepwise_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('bnnhar_stepwise') )
)

horizons_test <- models_predictions %>% 
  distinct( pred_horizon )

models_test <- models_predictions %>% 
  distinct( model )



info( logger, "HAR_NEURAL_PROJECT::combine models" )

all_combinations <- foreach ( i = 2:length(models_test$model) ) %do% {
  
  combn( x = models_test$model, m = i ) %>% t %>% as_data_frame()
}

all_comb <- do.call( bind_rows, all_combinations)

combinations_forecast <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach ( i = 1:dim(all_comb)[1], .combine = rbind ) %dopar% {
    
    models <- all_comb %>% slice(i) %>% t %>% c
    
    combinations_models <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      filter( model %in% models ) %>% 
      spread( key = model, value = prediction )
    
    average <- rowMeans(combinations_models %>% select(-date, -rv5_252, -pred_horizon))
    names <- colnames(combinations_models %>% select(-date, -rv5_252, -pred_horizon))
    
    data_frame( date = combinations_models$date,
                rv5_252 = combinations_models$rv5_252,
                prediction = average,
                pred_horizon = horizons,
                model = paste( names, collapse = "+"))
    
  }

models_predictions %<>% 
  bind_rows(., combinations_forecast )

models_test <- models_predictions %>% 
  distinct( model )
