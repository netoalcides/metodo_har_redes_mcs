info( logger, "HAR_NEURAL_PROJECT::clean na" )

bvsp_rv5 %<>% 
  na.omit()

info( logger, "HAR_NEURAL_PROJECT::adjust rv 252 days" )

bvsp_rv5 %<>% 
  mutate( rv5_252 = rv5 * 252 ) %>% 
  select( -rv5 )
