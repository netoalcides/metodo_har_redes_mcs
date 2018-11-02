info( logger, "HAR_NEURAL_PROJECT::load_dataset" )

load("data/oxfordmanrealizedvolatilityindices.RData")

bvsp_rv5 <- oxfordmanrealizedvolatilityindices %>% 
  dplyr::filter( Symbol == ".BVSP" ) %>% 
  select( date, rv5 )

rm(oxfordmanrealizedvolatilityindices)