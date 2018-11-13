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


info( logger, "HAR_NEURAL_PROJECT::define simulation parameters" )

t_maturity = 22 - 1
tau = 0.1 # threshould
trading_costs = 0.025
# tau_sequence = seq( from = 0, to = 0.5, by = 0.05 )
# trading_costs_sequence = seq( from = 0, to = 0.025, by = 0.0025 )


info( logger, "HAR_NEURAL_PROJECT::option price simulation" )

options_price <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
    
    models_predictions %>% 
      filter( pred_horizon == horizons,
              model == models ) %>% 
      left_join(., bvsp_rv5 %>% select( date, close_price, cdi ),
                by = 'date' ) %>% 
      mutate( op_call = bs_call( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ),
              op_put = bs_put( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ) )
    
  }


mean_rv <- mean(har_original_data_structure_train$rv5_252) # helps threshould calculus


straddle_returns <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
    
    options_price %>% 
      filter( pred_horizon == horizons,
              model == models ) %>% 
      mutate( variation = (prediction - lag(rv5_252)) / mean_rv,
              strategy = straddle_strategy_applied( variation_expected = variation, threshould = tau ),
              returns = straddle_strategy_returns( rf = cdi,
                                                   trading_costs = trading_costs,
                                                   strategy = strategy,
                                                   p_call0 = lag(op_call),
                                                   p_put0 = lag(op_put),
                                                   p_call1 = op_call,
                                                   p_put1 = op_put ) )
    
  }

straddle_returns %>% 
  na.omit %>% 
  group_by( model, pred_horizon ) %>% 
  count( strategy ) %>% 
  print(n=Inf)

downside_risk <- function( x ){ sqrt(mean(pmin( 0, x )^2))}

omega_metric = function( return, mar){ sum( ifelse( return >= mar, return - mar, 0) ) / sum( ifelse( return < mar, mar - return, 0) )  }

straddle_returns %>% 
  na.omit %>% 
  group_by( model, pred_horizon ) %>% 
  summarise( sharpe_ratio = mean(returns - cdi) / sd(returns),
             sortino_ratio = mean(returns - cdi) / downside_risk(returns),
             omega_ratio =  omega_metric( return = returns, mar = cdi ) )






