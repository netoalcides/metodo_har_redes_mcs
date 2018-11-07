info( logger, "HAR_NEURAL_PROJECT::train and predict stepwise HAR model - fixed window" )

# setting up inital models
null <- lm( rv5_252 ~ 1, data = har_stepwise_data_structure_train )
full <- lm( rv5_252 ~ ., data = har_stepwise_data_structure_train %>%
              select( -date, -close_price, -log_return, -cdi ) )

# training BIC method
fixed_window_har_stepwise_bic <- step(null, 
                                      scope = list(upper=full), 
                                      direction = "both",  
                                      k = log(T_training), 
                                      trace = 0)

# training AIC method
fixed_window_har_stepwise_aic <- step(null, 
                                      scope = list(upper=full), 
                                      direction = "both",  
                                      k = 2, 
                                      trace = 0)
