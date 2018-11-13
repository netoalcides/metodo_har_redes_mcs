info( logger, "HAR_NEURAL_PROJECT::Loss functions analysis" )

# carrega dados
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



# test
models_loss <- foreach( horizon = horizons_test$pred_horizon, .combine = rbind ) %dopar% {
  
  models_predictions %>% 
    filter( pred_horizon == horizon ) %>% 
    group_by( model ) %>% 
    summarise( rmse = rmse(pred = prediction, real = rv5_252 ),
               mae = mae(pred = prediction, real = rv5_252 ),
               haae = haae(pred = prediction, real = rv5_252 ),
               qlike = qlike(pred = prediction, real = rv5_252 ),
               le = le(pred = prediction, real = rv5_252 ) ) %>% 
    mutate( pred_horizon = horizon )
  
}
