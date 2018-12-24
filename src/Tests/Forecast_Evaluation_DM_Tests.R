info( logger, "HAR_NEURAL_PROJECT::Diebold Mariano analysis" )

compare_models <- combn( x = models_test$model, m = 2 )

diebold_mariano_tests <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( compare = 1:dim(compare_models)[2], .combine = rbind ) %dopar% {
    
    e_1 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      mutate( models_error = prediction - rv5_252 ) %>% 
      filter( model == compare_models[1, compare] )
    
    e_2 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      mutate( models_error = prediction - rv5_252 ) %>% 
      filter( model == compare_models[2, compare] )
    
    h_ <- str_extract( string = horizons, 
                       pattern = '(\\d)+') %>% 
      as.numeric()
    
    diebold_mariano <- dm.test( e1 = e_1$models_error, 
                                e2 = e_2$models_error, 
                                h = h_, 
                                power = 2)
    
    model_1_id <- models_id %>% 
      filter( model == compare_models[1, compare] )
    
    model_2_id <- models_id %>% 
      filter( model == compare_models[2, compare] )
    
    data_frame( model_1 = compare_models[1, compare],
                id_model_1 = model_1_id$id_model,
                model_1_n = e_1$n_models[1],
                model_2 = compare_models[2, compare],
                id_model_2 = model_2_id$id_model,
                model_2_n = e_2$n_models[1],
                pred_horizon = horizons,
                dm_test = paste0( round(diebold_mariano$statistic, 3), 
                                  '(', round(diebold_mariano$p.value, 3), ')' ),
                result = ifelse( diebold_mariano$statistic > 0, 'best_model_2', 'best_model_1'),
                sig = ifelse( diebold_mariano$p.value < 0.01, 'sig_1%', 
                              ifelse( diebold_mariano$p.value >= 0.01 & diebold_mariano$p.value < 0.05, 'sig_5%', 
                                      ifelse( diebold_mariano$p.value >= 0.05 & diebold_mariano$p.value < 0.1, 'sig_10%', 'no_sig' ) ) ) )
} 

cache( 'diebold_mariano_tests' )

