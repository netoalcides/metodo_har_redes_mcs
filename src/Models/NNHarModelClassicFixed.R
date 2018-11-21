info( logger, "HAR_NEURAL_PROJECT::train and predict classic NNHAR model - fixed window" )





function( x, use_sample_parameters = c('no', 'yes'), m = NULL, s = NULL ){ 
  
  if( use_sample_parameters == 'no' ){
    
    if( is.null(m) == TRUE | is.null(s) == TRUE ){ stop( 'please input m and s' ) }
    
    (x - m) / s
    
  } else {
    
    (x - mean(x)) / sd(x)
    
  }
  
}


z = x - m / s

z*s = x - m

x = m + z*s

denormalize <- function(z, m, s){ m + z*s }



info( logger, "HAR_NEURAL_PROJECT::normalize data - fixed window" )

sample_size_percentage_neuralnet = 0.8125
T_training_neuralnet <- round( T_training * sample_size_percentage_neuralnet )

neural_data_train <- har_original_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'yes') ) ) %>% 
  slice( 1:T_training_neuralnet )

neural_data_test <- har_original_data_structure_train %>% 
  mutate( log_rv252 = log(rv5_252) ) %>% 
  transmute_at( vars( contains('log_') ), 
                funs( normalization( x = ., use_sample_parameters = 'yes') ) ) %>% 
  slice( (T_training_neuralnet+1):T )



info( logger, "HAR_NEURAL_PROJECT::set tunning parameters - fixed window" )

set.seed(12345)

number_tunning_run = 3
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
                       m = mean( log(har_original_data_structure_train$rv5_252) ), 
                       s = sd( log(har_original_data_structure_train$rv5_252) ) )
  
  real <- denormalize( z = neural_data_test$log_rv252, 
               m = mean( log(har_original_data_structure_train$rv5_252) ), 
               s = sd( log(har_original_data_structure_train$rv5_252) ) )
  
  # calculate error
  rmse <- sqrt( mean( ( exp(pred) - exp(neural_data_test$log_rv252))^2 ) )
  
  data_frame( id = running,
              size = size_[running], 
              decay = decay_[running],
              maxit = maxit_[running],
              abstol = abstol_[running],
              reltol = reltol_[running],
              rmse = rmse )
  
} 





