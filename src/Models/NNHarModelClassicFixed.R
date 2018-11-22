info( logger, "HAR_NEURAL_PROJECT::train and predict classic NNHAR model - fixed window" )



info( logger, "HAR_NEURAL_PROJECT::normalize data - fixed window" )

sample_mean = mean( log(har_original_data_structure$rv5_252) )
sample_std_dev = sd( log(har_original_data_structure$rv5_252) )

sample_size_percentage_neuralnet = 0.8125
T_training_neuralnet <- round( T_training * sample_size_percentage_neuralnet )

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

set.seed(12345)

number_tunning_run = 150
size_ = sample( 3:10, number_tunning_run, replace = TRUE) 
decay_ = runif(min = 0, max = 0.001, n = number_tunning_run)
maxit_ = sample( 150:500, number_tunning_run, replace = TRUE )
abstol_ = runif(min = 0.00001, max = 0.0009, n = number_tunning_run)
reltol_ = runif(min = 0.000000001, max = 0.00000009, n = number_tunning_run)


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
  rmse <- sqrt( mean( (exp(pred) - exp(real))^2 ) )
  
  data_frame( id = running,
              size = size_[running], 
              decay = decay_[running],
              maxit = maxit_[running],
              abstol = abstol_[running],
              reltol = reltol_[running],
              rmse = rmse )
  
} 

best <- model_tunning %>% 
  arrange( rmse ) %>% 
  slice(1)



info( logger, "HAR_NEURAL_PROJECT::train best model - fixed window" )
set.seed(12345)
model <- nnet( log_rv252~., 
               data = neural_data_train, 
               linout = TRUE,
               trace = F,
               size = best$size, 
               decay = best$decay,
               maxit = best$maxit,
               abstol = best$abstol,
               reltol = best$reltol )



info( logger, "HAR_NEURAL_PROJECT::forecasting NNHar classic - fixed window" )

results_forecasts_nnhar_classic <- foreach( horizons = c(1, 5, 10, 15) ) %:%
  foreach( rolling_valid_sample = 1:( ceiling( (T-T_training)/horizons) ), .combine='rbind' ) %dopar% {
    
    # prepare data to forecasts
    data_to_loop <- har_original_data_structure %>%
      slice( (T_training - 21):(T_training + ( (rolling_valid_sample-1)*horizons ) + 1 ) ) %>%
      select( date, rv5_252 )
    
    
    # loop recursive forecasts
    forecasts <- har_forecast( model = fixed_window_har_original_model,
                               x_reg = data_to_loop,
                               horizons = horizons,
                               har_lag_structure = har_original_lag_structure,
                               har_dataset =  har_original_data_structure, 
                               model_type =  'log' )
    
    forecasts %>%
      mutate( pred_horizon = paste0('h_', horizons ) )
    
  }



function(model, x_reg, har_lag_structure, har_dataset, horizons, model_type = c('level', 'sqrt', 'log') ){
  
  forecasts <- NULL
  pred <- NULL
  
  for( h in 1:horizons){
    
    new_data <- x_reg %>%
      bind_cols(., har_structure( x_reg$rv5_252, har_lag_structure, model_type ) ) %>%
      na.omit
    
    pred <- predict( model, new_data %>% last )
    
    pred <- ifelse( model_type == 'sqrt', pred^2, 
                    ifelse( model_type == 'log', exp(pred), 
                            pred ) )
    
    last_point <- dim(x_reg)[1]
    
    x_reg$rv5_252[last_point] <- pred
    
    forec <- bind_cols( new_data %>%
                          last %>%
                          select( date, rv5_252 ),
                        data_frame( prediction = pred ) )
    
    forecasts %<>%
      bind_rows(., forec)
    
    new_x <- har_dataset %>%
      filter( date > last(x_reg$date) ) %>%
      slice(1) %>%
      select( date, rv5_252 )
    
    x_reg %<>%
      bind_rows(., new_x )
    
  }
  
  return( forecasts )
}









