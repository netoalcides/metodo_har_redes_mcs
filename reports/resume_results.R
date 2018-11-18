
summary(fixed_window_har_original_model)

summary(fixed_window_har_stepwise_bic)

summary(fixed_window_har_stepwise_aic)

models_predictions %>% count(model, pred_horizon)

models_loss %>% split( .$pred_horizon )

models_mzreg %>% split( .$pred_horizon )

diebold_mariano_tests %>% split( .$pred_horizon )

giacomini_white_tests %>% split( .$pred_horizon )

var_test_results %>% split( .$pred_horizon )

straddle_strategy_results %>% split( .$pred_horizon )



