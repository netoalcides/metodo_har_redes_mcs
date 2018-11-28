info( logger, "HAR_NEURAL_PROJECT::train and predict classic NNHAR model - fixed window" )



info( logger, "HAR_NEURAL_PROJECT::normalize data - fixed window" )

neural_data_train <- har_original_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'no', 
                                     m = sample_mean, s = sample_std_dev) ) ) %>% 
  slice( 1:T_training_neuralnet )

neural_data_test <- har_original_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'no', 
                                     m = sample_mean, s = sample_std_dev) ) ) %>% 
  slice( (T_training_neuralnet+1):T )



info( logger, "HAR_NEURAL_PROJECT::set tunning parameters - fixed window" )

model_tunning <- foreach( running = 1:number_tunning_run, .combine = rbind ) %dopar% {
  
  # train model
  set.seed(12345)
  model <- brnn( formula = log_rv252~., 
                 data = neural_data_train,
                 normalize = FALSE,
                 verbose = F,
                 neurons = size_[running],
                 epochs = maxit_[running],
                 mu = decay_[running],
                 change = abstol_[running],
                 tol = reltol_[running])
  
  # predict test data
  pred <- predict( model, newdata = neural_data_test )
  pred <- denormalize( z = pred, 
                       m = sample_mean, 
                       s = sample_std_dev)
  
  real <- denormalize( z = neural_data_test$log_rv252, 
                       m = sample_mean, 
                       s = sample_std_dev )
  
  # calculate error
  mse <- mean( (exp(pred) - exp(real))^2 )
  n_weigths <- model$npar
  aic_ <- length(real) + length(real)*log(2*pi) + length(real)*log(mse) + 2*(n_weigths+1)
  
  data_frame( id = running,
              size = size_[running], 
              decay = decay_[running],
              maxit = maxit_[running],
              abstol = abstol_[running],
              reltol = reltol_[running],
              aic = aic_ )
  
  
}
  
best <- model_tunning %>% 
  arrange( aic ) %>% 
  slice(1)



info( logger, "HAR_NEURAL_PROJECT::train best model - fixed window" )
set.seed(12345)
fixed_window_bnnhar_original_model <- brnn( formula = log_rv252~., 
               data = neural_data_train,
               normalize = FALSE,
               verbose = F,
               neurons = best$size,
               epochs = best$maxit,
               mu = best$decay,
               change = best$abstol,
               tol = best$reltol )



info( logger, "HAR_NEURAL_PROJECT::forecasting BNNHar classic - fixed window" )

results_forecasts_bnnhar_classic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( ceiling( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {
    
    # prepare data to forecasts
    data_to_loop <- har_original_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )
    
    
    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_bnnhar_original_model,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_original_lag_structure,
                               har_dataset =  har_original_data_structure, 
                               model_type =  'log', 
                               normalize =  TRUE )
    
    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )
    
}



info( logger, "HAR_NEURAL_PROJECT::adjust results - fixed window" )

results_forecasts_bnnhar_classic_by_horizon.fixed_window <- bind_rows(results_forecasts_bnnhar_classic) %>%
  group_by( date, pred_horizon ) %>%
  filter( row_number() <= 1 ) %>%
  ungroup()

cache("results_forecasts_bnnhar_classic_by_horizon.fixed_window")

rm( neural_data_train, neural_data_test, model_tunning, best, results_forecasts_bnnhar_classic )

