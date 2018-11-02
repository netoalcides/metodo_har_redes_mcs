info( logger, "HAR_NEURAL_PROJECT::transform" )

bvsp_rv5 %<>% 
  mutate( log_return = log( close_price/ lag(close_price) ),
          rv5_252 = rv5 * 252 ) %>% 
  select( -rv5 )


info( logger, "HAR_NEURAL_PROJECT::clean na" )

bvsp_rv5 %<>% 
  na.omit()


