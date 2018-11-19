# https://eranraviv.com/volatility-forecast-evaluation-in-r/
# https://github.com/cran/afmtools/blob/master/R/gw.test.R
# https://insightr.wordpress.com/tag/diebold-mariano-test/



# carrega dados
list.files( "./cache", pattern = 'fixed_window.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )

rmse <- function( pred, real ){ sqrt( mean( (pred - real)^2 ) ) }

# dados

models_predictions_train <- har_original_data_structure_train %>% 
  dplyr::select( date, rv5_252) %>% 
  mutate( har_classic = fixed_window_har_original_model$fitted.values,
          har_stepwise_aic = fixed_window_har_stepwise_aic$fitted.values,
          har_stepwise_bic = fixed_window_har_stepwise_bic$fitted.values ) %>% 
  gather( key = model, value = prediction, -c(date:rv5_252) )

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


# RMSE
# train
models_predictions_train %>% 
  group_by( model ) %>% 
  summarise( rmse = rmse(pred = prediction, real = rv5_252) )

# test
models_rmse <- foreach( horizon = horizons_test$pred_horizon, .combine = rbind ) %dopar% {
  
  models_predictions %>% 
    filter( pred_horizon == horizon ) %>% 
    group_by( model ) %>% 
    summarise( rmse = rmse(pred = prediction, real = rv5_252 ) ) %>% 
    mutate( pred_horizon = horizon )
  
}
models_rmse

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


# diebold-mariano

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

# diebold_mariano_tests %>% 
#   filter( pred_horizon == 'h_1' ) %>% 
#   select( -pred_horizon ) %>% 
#   spread( key = model_2, value = dm_test )


# giacomini-white

giacomini_white_tests <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( compare = 1:dim(compare_models)[2], .combine = rbind ) %dopar% {
    
    f_1 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      filter( model == compare_models[1, compare] )
    
    f_2 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      filter( model == compare_models[2, compare] )
    
    h_ <- str_extract( string = horizons, 
                       pattern = '(\\d)+') %>% 
      as.numeric()
    
    giacomini_white <- gw.test( x = f_1$prediction, 
                                y = f_2$prediction, 
                                p = f_1$rv5_252, 
                                T = length(f_1$rv5_252),
                                tau = h_, 
                                method = "HAC", 
                                alternative = "two.sided")
    
    data_frame( model_1 = compare_models[1, compare],
                model_2 = compare_models[2, compare],
                pred_horizon = horizons,
                gw_test = paste0( round(giacomini_white$statistic, 3), 
                                  '(', round(giacomini_white$p.value, 3), ')' ),
                result = ifelse( giacomini_white$statistic > 0, 'best_model_2', 'best_model_1'),
                sig = ifelse( giacomini_white$p.value < 0.05, 'sig.', 'no_sig.') )
  }






















