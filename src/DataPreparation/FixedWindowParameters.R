info( logger, "HAR_NEURAL_PROJECT::window methodology parameters" )

info( logger, "HAR_NEURAL_PROJECT::fixed window" )
sample_size_percentage <- sample_percentage



info( logger, "HAR_NEURAL_PROJECT::Classic Har lag structure" )

har_original_lag_structure <- c(1, 5, 22)

har_original_data_structure <- bvsp_rv5 %>% 
  bind_cols(., har_structure( bvsp_rv5$rv5_252, har_original_lag_structure, model_type = 'log') ) %>% 
  na.omit

# Size fixed window parameters
T <- dim(har_original_data_structure)[1]
T_training <- round( T * sample_size_percentage )

# Train and validation data
har_original_data_structure_train <- har_original_data_structure %>% 
  slice( 1:T_training )

har_original_data_structure_valid <- har_original_data_structure %>% 
  slice( (T_training+1):T )



info( logger, "HAR_NEURAL_PROJECT::Har Stepwise lag structure" )

har_stepwise_lag_structure <- c(1:22)

har_stepwise_data_structure <- bvsp_rv5 %>% 
  bind_cols(., har_structure( bvsp_rv5$rv5_252, har_stepwise_lag_structure, model_type = 'log') ) %>% 
  na.omit

# Train and validation data
har_stepwise_data_structure_train <- har_stepwise_data_structure %>% 
  slice( 1:T_training )

har_stepwise_data_structure_valid <- har_stepwise_data_structure %>% 
  slice( (T_training+1):T )



info( logger, "HAR_NEURAL_PROJECT::NNHar parameters" )

# normalization parameters
sample_mean = mean( log(har_original_data_structure$rv5_252) )
sample_std_dev = sd( log(har_original_data_structure$rv5_252) )

# sample parameters
sample_size_percentage_neuralnet = 0.8125
T_training_neuralnet <- round( T_training * sample_size_percentage_neuralnet )

# tunning parameters
set.seed(12345)
number_tunning_run = 150
size_ = sample( 3:10, number_tunning_run, replace = TRUE) 
decay_ = runif(min = 0, max = 0.001, n = number_tunning_run)
maxit_ = sample( 150:500, number_tunning_run, replace = TRUE )
abstol_ = runif(min = 0.00001, max = 0.0009, n = number_tunning_run)
reltol_ = runif(min = 0.000000001, max = 0.00000009, n = number_tunning_run)




