info( logger, "HAR_NEURAL_PROJECT::Diebold Mariano analysis" )

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


# tests
compare_models <- combn( x = models_test$model, m = 2 )

diebold_mariano_tests <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( compare = 1:dim(compare_models)[2], .combine = rbind ) %dopar% {
    
    e_1 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      mutate( models_error = prediction - rv5_252 ) %>% 
      filter( model == compare_models[1, compare] )
    
    e_2 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      mutate( models_error = prediction - rv5_252 ) %>% 
      filter( model == compare_models[2, compare] )
    
    h_ <- str_extract( string = horizons, 
                       pattern = '(\\d)+') %>% 
      as.numeric()
    
    diebold_mariano <- dm.test( e1 = e_1$models_error, 
                                e2 = e_2$models_error, 
                                h = h_, 
                                power = 2)
    
    data_frame( model_1 = compare_models[1, compare],
                model_2 = compare_models[2, compare],
                pred_horizon = horizons,
                dm_test = paste0( round(diebold_mariano$statistic, 3), 
                                  '(', round(diebold_mariano$p.value, 3), ')' ),
                result = ifelse( diebold_mariano$statistic > 0, 'best_model_2', 'best_model_1'),
                sig = ifelse( diebold_mariano$p.value < 0.05, 'sig.', 'no_sig.') )
  } 