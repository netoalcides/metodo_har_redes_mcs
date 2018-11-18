info( logger, "HAR_NEURAL_PROJECT::options trading simulation analysis" )

info( logger, "HAR_NEURAL_PROJECT::define simulation parameters" )

# t_maturity = 22 - 1
# tau = 0.05 # threshould
# trading_costs = 0.0100
# # tau_sequence = seq( from = 0, to = 0.5, by = 0.05 )
# # trading_costs_sequence = seq( from = 0, to = 0.025, by = 0.0025 )



info( logger, "HAR_NEURAL_PROJECT::option price simulation" )

options_price <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
    
    models_predictions %>% 
      filter( pred_horizon == horizons,
              model == models ) %>% 
      left_join(., bvsp_rv5 %>% select( date, close_price, simple_return, cdi ),
                by = 'date' ) %>% 
      mutate( op_call = bs_call( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ),
              op_put = bs_put( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ) )
    
  }



info( logger, "HAR_NEURAL_PROJECT::straddle strategy applied" )

mean_rv <- mean(har_original_data_structure_train$rv5_252) # helps threshould calculus

straddle_returns <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( models = models_test$model, .combine = rbind ) %dopar% {
    
    options_price %>% 
      filter( pred_horizon == horizons,
              model == models ) %>% 
      mutate( variation = (prediction - lag(rv5_252)) / mean_rv,
              strategy = straddle_strategy_applied( variation_expected = variation, threshould = tau ),
              returns_options = straddle_strategy_returns( rf = cdi,
                                                   trading_costs = trading_costs,
                                                   strategy = strategy,
                                                   p_call0 = lag(op_call),
                                                   p_put0 = lag(op_put),
                                                   p_call1 = op_call,
                                                   p_put1 = op_put ) )
    
  }

straddle_returns %<>% 
  na.omit
  


info( logger, "HAR_NEURAL_PROJECT::calculates strategy performance" )

decisions <- straddle_returns %>% 
  group_by( model, pred_horizon ) %>% 
  count( strategy ) %>% 
  spread( key = strategy, value = n ) %>% 
  ungroup()

performance_metrics <- straddle_returns %>% 
  group_by( model, pred_horizon ) %>% 
  summarise( sharpe_ratio = mean(returns_options - cdi) / sd(returns_options),
             sortino_ratio = mean(returns_options - cdi) / downside_risk(returns_options),
             omega_ratio =  omega_ratio( return = returns_options, mar = cdi ),
             alpha_ratio = jensen_alpha( portfolio_return = returns_options, market_return = simple_return, rf = cdi) ) %>% 
  ungroup()

straddle_strategy_results <- decisions %>% 
  bind_cols(., performance_metrics %>% 
              select( -model, -pred_horizon ) )
