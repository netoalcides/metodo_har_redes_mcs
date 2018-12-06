info( logger, "HAR_NEURAL_PROJECT::train and predict stepwise NNHAR model - fixed window" )



info( logger, "HAR_NEURAL_PROJECT::normalize data - fixed window" )

sample_mean = mean( log(har_stepwise_data_structure_train$rv5_252) )
sample_std_dev = sd( log(har_stepwise_data_structure_train$rv5_252) )

sample_size_percentage_neuralnet = 0.8125
T_training_neuralnet <- round( T_training * sample_size_percentage_neuralnet )

neural_data_train <- har_stepwise_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'no', 
                                     m = sample_mean, s = sample_std_dev) ) ) %>% 
  slice( 1:T_training_neuralnet )

neural_data_test <- har_stepwise_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'no', 
                                     m = sample_mean, s = sample_std_dev) ) ) %>% 
  slice( (T_training_neuralnet+1):T )



info( logger, "HAR_NEURAL_PROJECT::set tunning parameters - fixed window" )

model_tunning <- foreach( running = 1:number_tunning_run, .combine = rbind ) %dopar% {
  
  # train model
  set.seed(12345)
  model <- nnet( log_rv252~., 
        data = neural_data_train, 
        linout = TRUE,
        trace = F,
        size = size_[running], 
        decay = decay_[running],
        maxit = maxit_[running],
        abstol = abstol_[running],
        reltol = reltol_[running] )
  
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
  n_weigths <- length(model$wts)
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
fixed_window_nnhar_stepwise_model <- nnet( log_rv252~., 
               data = neural_data_train, 
               linout = TRUE,
               trace = F,
               size = best$size, 
               decay = best$decay,
               maxit = best$maxit,
               abstol = best$abstol,
               reltol = best$reltol )



info( logger, "HAR_NEURAL_PROJECT::average partial effects - fixed window" )

neural_data_train <- har_stepwise_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'no', 
                                     m = sample_mean, s = sample_std_dev) ) )

set.seed(12345)
nnhar_stepwise_model <- nnet( log_rv252~., 
                              data = neural_data_train, 
                              linout = TRUE,
                              trace = F,
                              size = best$size, 
                              decay = best$decay,
                              maxit = best$maxit,
                              abstol = best$abstol,
                              reltol = best$reltol )

variables <- nnhar_stepwise_model$coefnames

fixed_window_nnhar_stepwise_model.average_effect <- foreach( b = 1:B, .combine = rbind ) %:% 
  foreach( v = 1:length(variables), .combine = rbind ) %dopar% {
    
    dados <- har_stepwise_data_structure_train %>% 
      mutate( log_rv252 = log(rv5_252) ) %>% 
      as.data.frame() %>% 
      slice( (endpoint[b]-block_size):endpoint[b] )
    
    ape_ <- average_partial_effect( variable = variables[v],
                                    model = nnhar_stepwise_model,
                                    data = dados,
                                    model_type = 'log' )
    
    data_frame( variavel = variables[v],
                ape = ape_)
    
  } 

fixed_window_nnhar_stepwise_model.average_effect %<>% 
  group_by( variavel ) %>% 
  summarise( ape_boot = mean(ape),
             q_5 = stats::quantile( ape, 0.025 ),
             q_95 = stats::quantile( ape, 0.975 ) ) %>% 
  mutate( results = paste0( round(ape_boot, 3), " (", round(q_5, 3), ", ", round(q_95, 3), ")") )



info( logger, "HAR_NEURAL_PROJECT::forecasting NNHar classic - fixed window" )

results_forecasts_nnhar_stepwise <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( ceiling( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {
    
    # prepare data to forecasts
    data_to_loop <- har_stepwise_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )
    
    
    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_nnhar_stepwise_model,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_stepwise_lag_structure,
                               har_dataset =  har_stepwise_data_structure, 
                               model_type =  'log', 
                               normalize =  TRUE )
    
    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )
    
  }


info( logger, "HAR_NEURAL_PROJECT::adjust results - fixed window" )

results_forecasts_nnhar_stepwise_by_horizon.fixed_window <- bind_rows(results_forecasts_nnhar_stepwise) %>%
  group_by( date, pred_horizon ) %>%
  filter( row_number() <= 1 ) %>%
  ungroup()

cache("results_forecasts_nnhar_stepwise_by_horizon.fixed_window")

rm( neural_data_train, neural_data_test, model_tunning, best, results_forecasts_nnhar_stepwise )







