info( logger, "HAR_NEURAL_PROJECT::Mincer Zarnowitz regressions analysis" )

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
