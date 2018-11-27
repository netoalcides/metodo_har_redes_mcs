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

# tunning parameters
set.seed(12345)
number_tunning_run = 150
size_ = sample( 3:10, number_tunning_run, replace = TRUE) 
decay_ = runif(min = 0, max = 0.001, n = number_tunning_run)
maxit_ = sample( 150:500, number_tunning_run, replace = TRUE )
abstol_ = runif(min = 0.00001, max = 0.0009, n = number_tunning_run)
reltol_ = runif(min = 0.000000001, max = 0.00000009, n = number_tunning_run)


brnn(bvrv~., data = train, neurons=x, normalize=FALSE, epochs=100, verbose=FALSE)

set.seed(12345)
model <- brnn( formula = log_rv252~., 
      data = neural_data_train,
      normalize=FALSE,
      verbose = F,
      neurons = 3,
      mu = 0.005,
      mu_dec = 0.1,
      mu_inc = 10,
      mu_max = 1e10,
      change = 0.001,
      tol = 1e-06)

model$npar

