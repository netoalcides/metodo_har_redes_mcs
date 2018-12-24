# loss individual

models_loss %>% 
  mutate( id = paste0(id_model, pred_horizon) ) %>% 
  left_join(., models_mzreg %>% 
              mutate( id = paste0(id_model, pred_horizon) ) %>% 
              select( id, r2),
            by = 'id' ) %>% 
  filter( n_models == 1 ) %>% 
  mutate( h = as.numeric(str_extract_all( string = pred_horizon, pattern = '(\\d)+'))  ) %>% 
  arrange( h, id_model ) %>% 
  select( id_model, model, h, rmse, mae, haae, qlike, le, r2 ) %>% 
  print( n = Inf )


giacomini_white_tests %>% 
  filter( model_1_n == 1, 
          model_2_n == 1,
          pred_horizon == 'h_1' ) %>% 
  select( id_model_1, id_model_2, gw_test ) %>% 
  spread( key = id_model_1, value = gw_test )
  
models_id

# loss all - criar os scores

best <- models_loss %>% 
  mutate( id = paste0(id_model, pred_horizon) ) %>% 
  left_join(., models_mzreg %>% 
              mutate( id = paste0(id_model, pred_horizon) ) %>% 
              select( id, r2),
            by = 'id' ) %>% 
  group_by( pred_horizon ) %>% 
  mutate( perc = cut( mae, 
                      breaks = quantile( mae, 
                                         seq( 0, 1, by = 0.05 ) ),
                      include.lowest = TRUE,
                      labels = FALSE ) ) %>% 
  ungroup() %>% 
  mutate( h = as.numeric(str_extract_all( string = pred_horizon, pattern = '(\\d)+'))  ) %>% 
  filter( perc == 1 ) %>% 
  arrange( h, id_model ) %>% 
  select( id_model, model, pred_horizon, h, n_models, rmse, mae, haae, qlike, le, r2 ) 

best %>% 
  print(n=Inf)

best %>% 
  count(model) %>% 
  arrange(n)


foreach( m = 1:dim(best)[1] ) %do% {
  
  b <- best %>% 
    slice(m)
  
  m1 <- giacomini_white_tests %>% 
    filter( id_model_1 == b$id_model,
            model_2_n == 1,
            pred_horizon == b$pred_horizon ) %>% 
    select( model_1, model_2, gw_test ) %>% 
    spread( key = model_2, value = gw_test )
  
  m2 <- giacomini_white_tests %>% 
    filter( id_model_2 == b$id_model,
            model_1_n == 1,
            pred_horizon == b$pred_horizon ) %>% 
    select( model_1, model_2, gw_test ) %>% 
    spread( key = model_1, value = gw_test )
  
  list(m1, m2)
  
}

giacomini_white_tests %>% 
  filter( id_model_1 == 12,
          model_2_n == 1,
          pred_horizon == 'h_1' )

giacomini_white_tests %>% 
  filter( id_model_2 == 12,
          model_1_n == 1,
          pred_horizon == 'h_1' ) %>% 
  select( model_1, model_2, gw_test ) %>% 
  spread( key = model_1, value = gw_test )
