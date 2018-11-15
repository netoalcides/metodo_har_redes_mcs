info( logger, "HAR_NEURAL_PROJECT::Processamento" )

registerDoFuture()
plan(multiprocess)

cat( 'HAR_NEURAL_PROJECT::loading data \n' )
source('src/DataPreparation/LoadDataset.R')


cat( 'HAR_NEURAL_PROJECT::ajusting data \n' )
source('src/DataPreparation/CleanTransform.R')


cat( 'HAR_NEURAL_PROJECT::window methodology parameters - preparing data for models \n' )
source('src/DataPreparation/FixedWindowParameters.R')


cat( 'HAR_NEURAL_PROJECT::estimating classic har model \n' )
source('src/Models/HarModelClassicFixed.R')

cat( 'HAR_NEURAL_PROJECT::estimating stepwise har model \n' )
source('src/Models/HarModelStepwiseFixed.R')

cat( 'HAR_NEURAL_PROJECT::prepare data for evaluation analysis \n' )
source( 'src/DataPreparation/EvaluationTestsData.R')

cat( 'HAR_NEURAL_PROJECT::loss functions evaluation \n' )
source( 'src/Tests/Forecast_Evaluation_Loss_Functions.R')


cat( 'HAR_NEURAL_PROJECT::mz regressions \n' )
source( 'src/Tests/Forecast_Evaluation_MZ_Regressions.R')


cat( 'HAR_NEURAL_PROJECT::diebold-mariano tests \n' )
source( 'src/Tests/Forecast_Evaluation_DM_Tests.R')


cat( 'HAR_NEURAL_PROJECT::giacomini-white tests \n' )
source( 'src/Tests/Forecast_Evaluation_GW_Tests.R')


cat( 'HAR_NEURAL_PROJECT::var fixed window \n' )
source('src/Tests/VaR_Analysis.R')


cat( 'HAR_NEURAL_PROJECT::options fixed window \n' )
source('src/Tests/Options_Trading_Analysis.R')
