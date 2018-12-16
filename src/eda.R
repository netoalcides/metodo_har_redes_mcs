# help functions
skewness <- function(x){ (sum((x - mean(x))^3)/length(x))/((sum((x - mean(x))^2)/length(x)))^(3/2) }
kurtosis <- function(x){ ((sum((x - mean(x))^4)/length(x))/ ((sum((x - mean(x))^2)/length(x))^2)) - 3 }


# descriptive table
har_original_data_structure %>% 
  mutate( logrv5 = log(rv5_252) ) %>%
  rename( rv5 = rv5_252) %>% 
  summarise_at( vars(rv5, logrv5),
                funs( mean, sd, skewness, kurtosis, min, max ) ) %>% 
  gather( key = stat, value = val ) %>% 
  separate( stat, into = c("var", "stat"), sep = "_" ) %>% 
  spread( key = var, value = val)



# serie line

har_original_data_structure %>% 
  ggplot( aes( x = date, y = rv5_252) ) +
  geom_line() +
  xlab('Data') +
  ylab('RV') +
  scale_x_date( date_breaks = "2 year", date_labels = "%Y" ) +
  theme_classic()


har_original_data_structure %>% 
  mutate( logrv5 = log(rv5_252) ) %>%
  ggplot( aes( x = date, y = logrv5) ) +
  geom_line() +
  xlab('Data') +
  ylab('log RV') +
  scale_x_date( date_breaks = "2 year", 
                date_labels = "%Y",
                limits = c( min(har_original_data_structure$date), NA) ) +
  theme_classic()


# histogram

har_original_data_structure %>% 
  ggplot( aes( x = rv5_252) ) +
  geom_histogram( color = 'black' ) +
  xlab('RV') +
  ylab('') +
  theme_classic()
  
har_original_data_structure %>% 
  mutate( logrv5 = log(rv5_252) ) %>%
  ggplot( aes( x = logrv5) ) +
  geom_histogram( color = 'black' ) +
  xlab('log RV') +
  ylab('') +
  theme_classic()


# acf, pacf

serie <- log(har_original_data_structure$rv5_252)
  
ci_line = qnorm((1 - 0.95)/2)/sqrt(length(serie))

acf( serie, lag.max = 40, plot = FALSE )$acf[,,1] %>% 
  as_tibble() %>% 
  rename( acf = value ) %>% 
  mutate( lag = row_number(),
          acf = ifelse( lag == 1, 0, acf ) ) %>% 
  ggplot( aes( x = lag, y = acf ) ) +
  geom_bar( stat = 'identity' ) +
  geom_hline( yintercept = ci_line, color = "blue", size = 0.3, linetype="dashed" ) +
  geom_hline( yintercept = -ci_line, color = "blue", size = 0.3, linetype="dashed" ) +
  xlab('Defasagem') +
  ylab('Autocorrelação') +
  theme_classic()

pacf( serie, lag.max = 40, plot = FALSE )$acf[,,1] %>% 
  as_tibble() %>% 
  rename( acf = value ) %>% 
  mutate( lag = row_number(),
          acf = ifelse( lag == 1, 0, acf ) ) %>% 
  ggplot( aes( x = lag, y = acf ) ) +
  geom_bar( stat = 'identity' ) +
  geom_hline( yintercept = ci_line, color = "blue", size = 0.3, linetype="dashed" ) +
  geom_hline( yintercept = -ci_line, color = "blue", size = 0.3, linetype="dashed" ) +
  xlab('Defasagem') +
  ylab('Autocorrelação Parcial') +
  theme_classic()
