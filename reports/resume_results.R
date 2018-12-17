
summary(fixed_window_har_original_model)
lmtest::coeftest( fixed_window_har_original_model, 
                  vcov. = NeweyWest(fixed_window_har_original_model, lag = 5) )


summary(fixed_window_har_stepwise_bic)
lmtest::coeftest( fixed_window_har_stepwise_bic, 
                  vcov. = NeweyWest(fixed_window_har_stepwise_bic, lag = 5) )

summary(fixed_window_har_stepwise_aic)
lmtest::coeftest( fixed_window_har_stepwise_aic, 
                  vcov. = NeweyWest(fixed_window_har_stepwise_aic, lag = 5) )


list.files( "./cache", pattern = 'average_effect.RData', full.names = TRUE ) %>% 
  lapply(., load, .GlobalEnv )


fixed_window_nnhar_original_model.average_effect

fixed_window_nnhar_stepwise_model.average_effect %>% 
  write_csv(., 'xx.csv')

fixed_window_bnnhar_original_model.average_effect

fixed_window_bnnhar_stepwise_model.average_effect %>% 
  write_csv(., 'xx.csv')




models_predictions %>% 
  filter( model == "har_classic", pred_horizon == "h_5" ) %>% 
  select( date, rv5_252, prediction ) %>% 
  gather( key = variaveis, value = valores, -date ) %>% 
  ggplot( aes( x = date, y = valores, colour = variaveis ) ) +
  geom_line()




models_predictions %>% count(model, pred_horizon)

models_loss %>% arrange( rmse ) %>% split( .$pred_horizon )

models_mzreg %>% arrange( desc(r2) ) %>% split( .$pred_horizon )

diebold_mariano_tests %>% arrange( desc(sig) ) %>% split( .$pred_horizon ) 

giacomini_white_tests %>% arrange( desc(sig) ) %>% split( .$pred_horizon )

var_normal_test_results %>% arrange(viol_ratio) %>% split( .$pred_horizon )

var_t_test_results %>% arrange(viol_ratio) %>% split( .$pred_horizon )

straddle_strategy_results %>% arrange( desc(sharpe_ratio) ) %>% split( .$pred_horizon )