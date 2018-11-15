info( logger, "HAR_NEURAL_PROJECT::VaR analysis" )

info( logger, "HAR_NEURAL_PROJECT::VaR analysis fixed window models" )

alpha_ = 0.05

models_var <- models_predictions %>% 
  mutate( prediction = ifelse( prediction < 0, 0, prediction ),
          VaR = -parametric_var(volatility = sqrt(prediction/252), alpha = alpha_ ) )

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
