info( logger, "HAR_NEURAL_PROJECT::VaR analysis" )

info( logger, "HAR_NEURAL_PROJECT::VaR analysis fixed window models" )

# carrega dados
list.files( "./cache", pattern = 'fixed_window.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )

# estima o var
models_var <- bind_rows( results_forecasts_har_classic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_classic') ),
                                 results_forecasts_har_stepwise_aic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_aic') ),
                                 results_forecasts_har_stepwise_bic_by_horizon.fixed_window %>% 
                                   mutate( model = paste0('har_stepwise_bic') )
)

alpha_ = 0.1

models_var %<>% 
  mutate( prediction = ifelse( prediction < 0, 0, prediction ),
          VaR = -parametric_var(volatility = sqrt(prediction/252), alpha = alpha_ ) )

horizons_test <- models_var %>% 
  distinct( pred_horizon )

models_test <- models_var %>% 
  distinct( model )

# realiza os testes
var_test_results <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
  
    var <- models_var %>% 
    filter( pred_horizon == horizons,
            model == models )
    
    var_test <- VaRTest( alpha = alpha_, 
             actual = har_original_data_structure_valid$log_return, 
             VaR = var$VaR,
             conf.level =  0.95 )
    
    data_frame( model = models,
                pred_horizon = horizons,
                viol_ratio = 100 * mean( ifelse( har_original_data_structure_valid$log_return < var$VaR, 1, 0 ) ),
                asmf = 100 * (sum( ifelse( har_original_data_structure_valid$log_return < var$VaR, (har_original_data_structure_valid$log_return - var$VaR)^2, 0 ) ) / sum( ifelse( har_original_data_structure_valid$log_return < var$VaR, 1, 0 ) )),
                lr_uc = paste0( round(var_test$uc.LRstat, 3), "(", round(var_test$uc.LRp, 3), ")" ),
                lr_cc = paste0( round(var_test$cc.LRstat, 3), "(", round(var_test$cc.LRp, 3), ")" ))
}

var_test_results


