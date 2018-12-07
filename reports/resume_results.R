
summary(fixed_window_har_original_model)

summary(fixed_window_har_stepwise_bic)

summary(fixed_window_har_stepwise_aic)

fixed_window_nnhar_original_model.average_effect
fixed_window_nnhar_stepwise_model.average_effect
fixed_window_bnnhar_original_model.average_effect
fixed_window_bnnhar_stepwise_model.average_effect




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