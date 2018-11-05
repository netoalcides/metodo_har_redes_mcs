info( logger, "HAR_NEURAL_PROJECT::load_dataset" )

load("data/oxfordmanrealizedvolatilityindices.RData")

cdi <- read_csv2( file = 'data/cdi.csv' )

bvsp_rv5 <- oxfordmanrealizedvolatilityindices %>% 
  dplyr::filter( Symbol == ".BVSP" ) %>% 
  select( date, close_price, rv5 )

rm(oxfordmanrealizedvolatilityindices)
