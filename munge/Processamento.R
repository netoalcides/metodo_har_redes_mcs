info( logger, "HAR_NEURAL_PROJECT::Processamento" )

cat( 'HAR_NEURAL_PROJECT::loading data \n' )
source('src/DataPreparation/LoadDataset.R')


cat( 'HAR_NEURAL_PROJECT::ajusting data \n' )
source('src/DataPreparation/CleanTransform.R')


cat( 'HAR_NEURAL_PROJECT::window methodology parameters - preparing data for models \n' )
source('src/DataPreparation/FixedWindowParameters.R')


cat( 'HAR_NEURAL_PROJECT::estimating classic har model \n' )
source('src/Models/HarModelClassicFixed.R')


cat( 'HAR_NEURAL_PROJECT::var fixed window \n' )
source('src/Tests/VaR_Analysis.R')

cat( 'HAR_NEURAL_PROJECT::options fixed window \n' )
source('src/Tests/Options_Trading_Analysis_HarModelClassicFixed.R')
