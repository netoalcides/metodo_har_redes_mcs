info( logger, "HAR_NEURAL_PROJECT::VaR analysis" )

info( logger, "HAR_NEURAL_PROJECT::VaR analysis fixed window models" )
load(file = "cache/fixed_window.results_forecasts_har_classic_by_horizon.RData")

fixed_window.var_forecasts_har_classic_by_horizon <- fixed_window.results_forecasts_har_classic_by_horizon %>%
  mutate( date = as_date(date) ) %>% 
  mutate_at( vars( contains('_h') ),
             funs( parametric_var(volatility = sqrt(./252), alpha = 0.05) ) ) %>% # parametric var
  rename_at( vars(contains('_h') ),
             funs( paste0('var_', .) ) ) %>% 
  left_join(., bvsp_rv5 %>% select( date, close_price, log_return ),
            by = 'date' ) %>% 
  select( -rv5_252, -close_price )

rm(fixed_window.results_forecasts_har_classic_by_horizon)