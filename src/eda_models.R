# loss individual

models_loss_individual <- models_loss %>% 
  mutate( id = paste0(id_model, pred_horizon) ) %>% 
  left_join(., models_mzreg %>% 
              mutate( id = paste0(id_model, pred_horizon) ) %>% 
              select( id, r2),
            by = 'id' ) %>% 
  filter( n_models == 1 ) %>% 
  mutate( h = as.numeric(str_extract_all( string = pred_horizon, pattern = '(\\d)+'))  ) %>% 
  arrange( h, id_model ) %>% 
  select( id_model, model, h, rmse, mae, haae, qlike, le, r2 ) %>% 
  group_by( h ) %>% 
  mutate( rmse_p = rank(rmse),
          mae_p = rank(mae),
          haae_p = rank(haae),
          qlike_p = rank(qlike),
          le_p = rank(le),
          r2_p = rank(1-r2),
          score = rank(rmse) + rank(mae) + 
            rank(haae) + rank(le) + rank(qlike) + 
            rank(1-r2),
          score_p = rank(score) )


models_loss_individual %>% 
  mutate( rmse = paste0(round(rmse*100,3), '(', rmse_p, ')' ),
          mae = paste0(round(mae*100,4), '(', mae_p, ')' ),
          haae = paste0(round(haae,4), '(', haae_p, ')' ),
          qlike = paste0(round(qlike,4), '(', qlike_p, ')' ),
          le = paste0(round(le,4), '(', le_p, ')' ),
          r2 = paste0(round(r2,4), '(', r2_p, ')' ),
          score = paste0(score, '(', score_p, ')' ) ) %>% 
  select( id_model, model, h, rmse, mae, haae, qlike, le, r2, score ) %>%
  write_csv(., path = 'xx.csv')
  print( n = Inf )




load( file = 'cache/giacomini_white_tests.RData' )


tests_individual <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %dopar% {
  
  giacomini_white_tests %>% 
    filter( model_1_n == 1, 
            model_2_n == 1,
            pred_horizon == horizons ) %>% 
    select( id_model_1, id_model_2, gw_test ) %>% 
    spread( key = id_model_1, value = gw_test ) %>% 
    mutate( h = as.numeric(str_extract_all( string = horizons, pattern = '(\\d)+')) )
  
} 


tests_individual %>% 
  write_csv(., path = 'xx.csv')
  
models_id

# loss all - criar os scores

models_loss_todos <- models_loss %>% 
  mutate( id = paste0(id_model, pred_horizon) ) %>% 
  left_join(., models_mzreg %>% 
              mutate( id = paste0(id_model, pred_horizon) ) %>% 
              select( id, r2),
            by = 'id' ) %>% 
  mutate( h = as.numeric(str_extract_all( string = pred_horizon, pattern = '(\\d)+'))  ) %>% 
  arrange( h, id_model ) %>% 
  select( id_model, model, n_models, h, rmse, mae, haae, qlike, le, r2 ) %>% 
  group_by( h ) %>% 
  mutate( rmse_p = rank(rmse),
          mae_p = rank(mae),
          haae_p = rank(haae),
          qlike_p = rank(qlike),
          le_p = rank(le),
          r2_p = rank(1-r2),
          score = rank(rmse) + rank(mae) + 
            rank(haae) + rank(le) + rank(qlike) + 
            rank(1-r2),
          score_p = rank(score) )

models_loss_todos %>% 
  mutate( rmse = paste0(round(rmse*100,3), ' (', rmse_p, ')' ),
          mae = paste0(round(mae*100,4), ' (', mae_p, ')' ),
          haae = paste0(round(haae,4), ' (', haae_p, ')' ),
          qlike = paste0(round(qlike,4), ' (', qlike_p, ')' ),
          le = paste0(round(le,4), ' (', le_p, ')' ),
          r2 = paste0(round(r2,4), ' (', r2_p, ')' ),
          score = paste0(score, ' (', score_p, ')' ) ) %>% 
  filter( n_models == 1 ) %>% 
  select( id_model, model, h, rmse, mae, haae, qlike, le, r2, score ) %>%
  print( n = Inf )




best <- models_loss_todos %>% 
  group_by( h ) %>% 
  mutate( perc = cut( score, 
                      breaks = quantile( score, 
                                         seq( 0, 1, by = 0.05 ) ),
                      include.lowest = TRUE,
                      labels = FALSE ) ) %>% 
  filter( perc == 1 ) %>% 
  ungroup()

best %>% 
  mutate( rmse = paste0(round(rmse*100,3), ' (', rmse_p, ')' ),
          mae = paste0(round(mae*100,4), ' (', mae_p, ')' ),
          haae = paste0(round(haae,4), ' (', haae_p, ')' ),
          qlike = paste0(round(qlike,4), ' (', qlike_p, ')' ),
          le = paste0(round(le,4), ' (', le_p, ')' ),
          r2 = paste0(round(r2,4), ' (', r2_p, ')' ),
          score = paste0(score, ' (', score_p, ')' ) ) %>% 
  select( id_model, model, h, rmse, mae, haae, qlike, le, r2, score ) %>% 
  write_csv(., path = 'xx.csv')
  print(n=Inf)

best %>% 
  count(model) %>% 
  arrange(n)


tests <- foreach( m = 1:dim(best)[1] ) %do% {
  
  b <- best %>% 
    slice(m)
  
  m1 <- giacomini_white_tests %>% 
    filter( id_model_1 == b$id_model,
            model_2_n == 1,
            pred_horizon == paste0('h_', b$h) ) %>% 
    select( model_1, model_2, gw_test ) %>% 
    spread( key = model_2, value = gw_test )
  
  m2 <- giacomini_white_tests %>% 
    filter( id_model_2 == b$id_model,
            model_1_n == 1,
            pred_horizon == paste0('h_', b$h) ) %>% 
    select( model_1, model_2, gw_test ) %>% 
    spread( key = model_1, value = gw_test )
  
  m1 %<>% 
    mutate( h = b$h )
  
  m2 %<>% 
    mutate( h = b$h )
  
  bind_rows(m1, m2)
  
}

bind_rows(tests) %>% 
  write_csv(., path = 'xx.csv')
  print(n=Inf)
