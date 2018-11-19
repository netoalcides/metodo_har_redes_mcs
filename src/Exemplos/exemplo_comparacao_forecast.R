#########################################################################
######################## Comparacao de forecasts ########################
#########################################################################

# exemplo do site
# https://insightr.wordpress.com/2017/11/09/formal-ways-to-compare-forecasting-models-rolling-windows/

#### Pacotes ####

library(AER)
library(xts)
library(foreach)
library(reshape2)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(doFuture)

#library(devtools)
#install_github("cran/afmtools")
library(afmtools)

#### uso do comando "embed" ####
x1 = c(1, 2, 3, 4, 5)
x2 = c(11, 12, 13, 14, 15)
x = cbind(x1, x2)

for (i in 1:3){
  
  show( embed(x, i) )
  
}

rm(i, x, x1, x2)

#### Prepara o modelo de forecast US inflation ####

### Dados ###
data("USMacroSWM")
data <- as.xts(USMacroSWM)[ , c("cpi", "production"), ]

# plot das series originais
data %>% 
  ggplot( aes( x = Index, y = cpi ) ) +
  geom_line() +
  xlab("years")

data %>% 
  ggplot( aes( x = Index, y = production ) ) +
  geom_line() +
  xlab("years")

# log e diff a série
data = cbind(diff(log(data[ ,"cpi"])), diff(log(data[ ,"production"])))[-1, ]

# plot das series difflog
data %>% 
  ggplot( aes( x = Index, y = cpi ) ) +
  geom_line() +
  xlab("years")

data %>% 
  ggplot( aes( x = Index, y = production ) ) +
  geom_line() +
  xlab("years")


### Cria as defasagens ###
lag = 4
X = embed(data, lag + 1)
X = as.data.frame(X)
colnames(X) = paste(rep(c("inf", "prod"), lag + 1),
                    sort(rep(paste("l", 0:lag, sep = ""),2)), sep = "" )
X$month = months(tail(index(data), nrow(X)))

head(X)

rm(lag, USMacroSWM)

#### Estima o modelo em janela deslizante - rolling window ####

### Define o tamanho fixo da janela ###

tamanho_janela <- 300
n_janelas <- nrow(X) - tamanho_janela

### Estima os modelos

registerDoFuture()
plan(multiprocess)

forecasts <- foreach( i = 1:n_janelas  ) %do% {
  
  # seleciona os dados para a estimacao e teste
  X_in <- X[ i:(tamanho_janela + i - 1), ]
  X_out <- X[ tamanho_janela + i, ]
  
  # estima o modelo 1 - regressao
  m1 <- lm( infl0 ~. - prodl0, data = X_in )
  f1 <- predict( m1, X_out )
  
  # estima modelo 2 - random walk
  f2 <- tail(X_in$infl0, 1)
  
  c(f1, f2)
}

forecasts <- do.call(rbind, forecasts)

#### Estima os erros ####

e1 <- tail( X[ ,"infl0"], nrow(forecasts) ) - forecasts[ ,1]
e2 <- tail( X[ ,"infl0"], nrow(forecasts) ) - forecasts[ ,2]

df = data.frame( "date" = tail( as.Date(index(data)), n_janelas ), 
                 "Regressao" = e1, 
                 "RandomWalk" = e2 )
df %>% 
  gather( key = modelo, 
          value = erros, 
          -date ) %>%
  ggplot( aes( x = date, 
               y = erros, 
               linetype = modelo, 
               color = modelo ) ) +
  geom_line()
  
#### Estima os RMSE e faz o teste de Giacomini e White por meio do teste de Diebold e Mariano ####

rmse1 <- 1000 * sqrt( mean(e1^2) )
rmse2 <- 1000 * sqrt( mean(e2^2) )

dm <- dm.test(e1 = e1, 
        e2 = e2, 
        alternative = "two.sided",
        h = 1, 
        power = 2 )

rmse1
rmse2
dm

# gw.test( x = forecasts[ ,1], 
#          y = forecasts[ ,2],
#          p = tail( X[ ,"infl0"], nrow(forecasts) ),
#          T = nrow(forecasts),
#          tau = 1,
#          alternative = "two.sided")
