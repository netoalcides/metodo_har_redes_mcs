info( logger, "HAR_NEURAL_PROJECT::Mincer Zarnowitz regressions analysis" )

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

# Mincer Zarnowitz regressions
models_mzreg <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
    
    regression <- lm( formula = rv5_252 ~ prediction, 
                      data =  models_predictions %>% 
                        filter( pred_horizon == horizons,
                                model == models ) )
    
    hypothesis_test <- linearHypothesis( regression, c("(Intercept) = 0", "prediction = 1"), white.adjust = TRUE )
    
    regression <- summary(regression)
    
    data_frame( model = models,
                pred_horizon = horizons,
                alpha = paste0( round(regression$coefficients[1], 4), "(", round( regression$coefficients[3], 4 ), ")" ),
                beta = paste0( round(regression$coefficients[2], 4), "(", round( regression$coefficients[3], 4 ), ")" ),
                joint = round(hypothesis_test$`Pr(>F)`[2], 3),
                r2 = round(regression$r.squared, 4) )
    
  }