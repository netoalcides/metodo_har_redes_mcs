info( logger, "HAR_NEURAL_PROJECT::Loss functions analysis" )

models_loss <- models_predictions %>% 
  group_by( model, pred_horizon, n_models ) %>% 
  summarise( rmse = rmse(pred = prediction, real = rv5_252 ),
             mae = mae(pred = prediction, real = rv5_252 ),
             haae = haae(pred = prediction, real = rv5_252 ),
             qlike = qlike(pred = prediction, real = rv5_252 ),
             le = le(pred = prediction, real = rv5_252 ) ) %>% 
  ungroup

models_loss %<>% 
  left_join(., models_id,
            by = 'model' )
