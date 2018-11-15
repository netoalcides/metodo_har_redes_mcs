
summary(fixed_window_har_original_model)

summary(fixed_window_har_stepwise_bic)

summary(fixed_window_har_stepwise_aic)

models_loss  %>% 
  filter( pred_horizon == 'h_1')

models_mzreg

diebold_mariano_tests

giacomini_white_tests

var_test_results

straddle_strategy_results %>% 
  filter( pred_horizon == 'h_15')
