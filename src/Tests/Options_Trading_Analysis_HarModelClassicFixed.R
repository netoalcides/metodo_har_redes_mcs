info( logger, "HAR_NEURAL_PROJECT::Option price analysis" )

load(file = "cache/fixed_window.results_forecasts_har_classic_by_horizon.RData")



info( logger, "HAR_NEURAL_PROJECT::define simulation parameters" )

#risk_free = 0.000246
t_maturity = 22 - 1
tau = 0.1 # threshould
trading_costs = 0.025
# tau_sequence = seq( from = 0, to = 0.5, by = 0.05 )
# trading_costs_sequence = seq( from = 0, to = 0.025, by = 0.0025 )



info( logger, "HAR_NEURAL_PROJECT::Option price simulation analysis fixed window models" )

economic_value_data <- har_original_data_structure_train %>% 
  tail(2) %>% 
  select( date, rv5_252 ) %>% 
  bind_rows(., fixed_window.results_forecasts_har_classic_by_horizon %>% 
              mutate( date = as_date(date) ) ) %>% 
  left_join(., bvsp_rv5 %>% select( date, close_price, cdi ),
          by = 'date' ) %>% 
  mutate( op_call = bs_call( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ),
          op_put = bs_put( S = close_price, K = lag(close_price), rf = cdi, h = t_maturity, sigma2 = rv5_252 ) )

mean_rv <- mean(har_original_data_structure_train$rv5_252) # helps threshould calculus

# add trading actions

economic_value_data %<>% 
  select( -contains('_h') ) %>% 
  bind_cols(., economic_value_data %>% 
              transmute_at( vars( contains('_h') ),
                            funs( (. - lag(rv5_252)) / mean_rv ) ) %>% 
              transmute_at( vars( contains('_h') ),
                            funs( straddle_strategy_applied( variation_expected = ., threshould = tau ) ) ) %>% 
              rename_at( vars(contains('_h') ),
                         funs( paste0('act_', .) ) )  )


# add trading returns

fixed_window.economic_value_data_har_classic_by_horizon <- economic_value_data %>% 
  bind_cols(., economic_value_data %>% 
              transmute_at( vars( contains('act_') ),
                            funs( straddle_strategy_returns( rf = cdi,
                                                             trading_costs = trading_costs,
                                                             strategy = .,
                                                             p_call0 = lag(op_call),
                                                             p_put0 = lag(op_put),
                                                             p_call1 = op_call,
                                                             p_put1 = op_put ) ) ) %>% 
              rename_at( vars( contains('act_') ), funs( paste0('ret_', .) ) ) ) %>% 
  na.omit()

rm(economic_value_data)


# 
# 
# fixed_window.economic_value_data_har_classic_by_horizon %>% 
#   select( contains('act_'), -contains('ret_act_') ) %>% 
#   map( ~count(data.frame(x=.x), x) )
# 
# 
# fixed_window.economic_value_data_har_classic_by_horizon %>% 
#   select( contains('ret_act_') ) %>% 
#   gather( key = variaveis, value = valores ) %>% 
#   ggplot( aes( x = valores ) ) +
#   geom_histogram() +
#   facet_grid( ~variaveis )
# 
# 
# fixed_window.economic_value_data_har_classic_by_horizon %>% 
#   select( cdi, contains('ret_act_') ) %>% 
#   summarise_at( vars(contains('ret_act_')),
#                 funs( (mean(.) - mean(cdi)) / sd(.) ) ) %>% 
#   rename_at( vars(contains('ret_act_')),
#              funs( paste0('fixed_window.returns_har_classic_by_horizon', str_extract(., '_h.*') ) ) ) %>% 
#   gather( key = modelos, value = sharpe_index )
# 
# 
# 
# downside_risk <- function( x ){ sqrt(mean(pmin( 0, x )^2))}
# 
# fixed_window.economic_value_data_har_classic_by_horizon %>% 
#   select( cdi, contains('ret_act_') ) %>% 
#   summarise_at( vars(contains('ret_act_')),
#                 funs( (mean(.) - mean(cdi)) / downside_risk(.) ) ) %>% 
#   rename_at( vars(contains('ret_act_')),
#              funs( paste0('fixed_window.returns_har_classic_by_horizon', str_extract(., '_h.*') ) ) ) %>% 
#   gather( key = modelos, value = sortino_index )





