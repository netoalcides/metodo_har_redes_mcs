info( logger, "HAR_NEURAL_PROJECT::transform" )


bvsp_rv5 %<>% 
  mutate( date = as_date(date),
          simple_return = (close_price - lag(close_price)) / lag(close_price),
          rv5_252 = rv5 * 252 ) %>% 
  left_join(., cdi %>% mutate( date = as_date(date) ),
            by = 'date' ) %>%
  mutate( cdi = na.approx(cdi) ) %>% 
  select( -rv5 )



info( logger, "HAR_NEURAL_PROJECT::clean na" )

bvsp_rv5 %<>% 
  na.omit()

rm(cdi)
