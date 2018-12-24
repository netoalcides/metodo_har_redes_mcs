info( logger, "HAR_NEURAL_PROJECT::Giacomini White analysis" )

compare_models <- combn( x = models_test$model, m = 2 )

giacomini_white_tests <- foreach( horizons = horizons_test$pred_horizon, .combine = rbind ) %:% 
  foreach( compare = 1:dim(compare_models)[2], .combine = rbind ) %dopar% {
    
    f_1 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      filter( model == compare_models[1, compare] )
    
    f_2 <- models_predictions %>% 
      filter( pred_horizon == horizons ) %>% 
      filter( model == compare_models[2, compare] )
    
    h_ <- str_extract( string = horizons, 
                       pattern = '(\\d)+') %>% 
      as.numeric()
    
    giacomini_white <- gw.test( x = f_1$prediction, 
                                y = f_2$prediction, 
                                p = f_1$rv5_252, 
                                T = length(f_1$rv5_252),
                                tau = h_, 
                                method = "NeweyWest", 
                                alternative = "two.sided")
    
    model_1_id <- models_id %>% 
      filter( model == compare_models[1, compare] )
    
    model_2_id <- models_id %>% 
      filter( model == compare_models[2, compare] )
    
    data_frame( model_1 = compare_models[1, compare],
                id_model_1 = model_1_id$id_model,
                model_1_n = f_1$n_models[1],
                model_2 = compare_models[2, compare],
                id_model_2 = model_2_id$id_model,
                model_2_n = f_2$n_models[1],
                pred_horizon = horizons,
                gw_test = paste0( round(giacomini_white$statistic, 3), 
                                  '(', round(giacomini_white$p.value, 3), ')' ),
                result = ifelse( giacomini_white$statistic > 0, 'best_model_2', 'best_model_1'),
                sig = ifelse( giacomini_white$p.value < 0.01, 'sig_1%', 
                              ifelse( giacomini_white$p.value >= 0.01 & giacomini_white$p.value < 0.05, 'sig_5%', 
                                      ifelse( giacomini_white$p.value >= 0.05 & giacomini_white$p.value < 0.1, 'sig_10%', 'no_sig' ) ) ) )
  }

cache( 'giacomini_white_tests' )
