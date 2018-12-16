# explorations

load("data/oxfordmanrealizedvolatilityindices.RData")

# using standard deviation
bvsp_rv5 <- oxfordmanrealizedvolatilityindices %>% 
  dplyr::filter( Symbol == ".BVSP" ) %>% 
  select( date, close_price, rv5 ) %>% 
  mutate( return = c(NA, diff(close_price, 1))/close_price,
          log_return = log( close_price/ lag(close_price) ),
          sd_rv5 = -sqrt(rv5) ) %>% 
  na.omit

sigma <- rollapply( bvsp_rv5$return, 120, function(x) sd(x) )

sigma <- c( rep(NA, dim(bvsp_rv5)[1] - length(sigma)),
            sigma )

bvsp_rv5 %<>% 
  mutate( sigma = -sigma ) %>% 
  na.omit


bvsp_rv5 %>% 
  select( date, return, sigma, sd_rv5 ) %>% 
  gather( key = variaveis, value = valores, -date ) %>% 
  ggplot( aes( x = date, y = valores, colour = variaveis ) ) +
  geom_line()

bvsp_rv5 %>% 
  select( date, log_return, sigma, sd_rv5 ) %>% 
  gather( key = variaveis, value = valores, -date ) %>% 
  ggplot( aes( x = date, y = valores, colour = variaveis ) ) +
  geom_line()

bvsp_rv5 %>% 
  select( date, sigma, sd_rv5 ) %>% 
  mutate( sigma = -sigma,
          sd_rv5 = -sd_rv5 ) %>% 
  gather( key = variaveis, value = valores, -date ) %>% 
  ggplot( aes( x = date, y = valores, colour = variaveis ) ) +
  geom_line()

bvsp_rv5 %>% 
  summarise( sd_sigma = sd(sigma),
             sd_sd_rv5 = sd(sd_rv5) )










