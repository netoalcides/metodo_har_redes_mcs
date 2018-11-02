###############################################
################# Exemplo VaR #################
###############################################

# http://pedrounb.blogspot.com/2012/08/value-at-risk-var-usando-o-r.html

# Habilita o pacote quantmod
library(quantmod)


## buscar dados

#Cria um novo ambiente para armazenar os dados
stockData <- new.env() 

#Especifica as datas de interesse
startDate = as.Date("2011-01-01") 
endDate = as.Date("2011-12-31")

#Obtêm os dados do ativo PETR4 e PETR3
getSymbols("PETR4.SA", 
           src = "yahoo",
           from = startDate,
           to = endDate)

#Calcula o log-retorno
retorno <- na.omit( diff( log( Cl( PETR4.SA ) ) ) )
rm(PETR4.SA)


### Estimar o VaR

## metodo NFHS (Normal Filtered Historical Simulation)
library(fGarch)
lambda <- 0.05


# Passo1: Encotrando os resíduos padronizados
fit <- arima( retorno, order=c(1,0,0) ) # estima model ARIMA
fit.res <- resid(fit) # obtem os residuos
modelo.N.FHS <- garchFit( ~garch(1, 1), 
                          fit.res, 
                          cond.dist = "norm" )
sresi <- (modelo.N.FHS@residuals / modelo.N.FHS@sigma.t)

#Passo2: Obtem-se sigma_t+1 e mu_t+1
mu.t1 <- predict(fit, n.ahead = 1)$pred[1]
sigma.t1 <- predict(modelo.N.FHS , n.ahead = 1)[1,3]

#Passo3: Para o período t+1 gera-se um conjunto de Retornos
uniforme <- ceiling( runif(1000)*length(sresi) )
Retornos <- (sresi[uniforme]*sigma.t1) + mu.t1

#Passo4: Calcular o VaR
VaR.t1.N.FHS <- -quantile(Retornos, lambda) 



## metodo STFHS (Student´s T Filtered Historical Simulation)
library(fGarch)
lambda <- 0.05

# Passo1: Encotrando os resíduos padronizados
fit <- arima( retorno, order=c(1,0,0) )
fit.res <- resid(fit)
modelo.S.FHS <- garchFit( ~garch(1, 1), 
                          fit.res, 
                          cond.dist = "sstd" )
sresi <- (modelo.S.FHS@residuals/modelo.S.FHS@sigma.t) 

# Passo2: Obtem-se sigma_t+1 e mu_t+1
mu.t1 <- predict(fit, n.ahead=1)$pred[1]
sigma.t1 <- predict(modelo.S.FHS, n.ahead=1)[1,1]

#Passo3: Para o período t+1 gera-se um conjunto de Retornos
uniforme <- ceiling( runif(1000)*length(sresi) )
Retornos <- (sresi[uniforme]*sigma.t1) + mu.t1

#Passo4: Calcular o VaR
VaR.t1.S.FHS <- -quantile(Retornos, lambda)



## metodo NEVT (Normal Extreme Value)
library(fGarch)
lambda<-0.05

#Passo1: Encotrando os resíduos padronizados
fit <- arima( retorno, order=c(1,0,0) )
fit.res <- resid(fit)
modelo.N.EVT <- garchFit( ~garch(1, 1), 
                          fit.res, 
                          cond.dist = "norm" )
sresi <- ( modelo.N.EVT@residuals/modelo.N.EVT@sigma.t ) 

#Passo2: Obtem-se sigma_t+1 e mu_t+1
mu.t1 <- predict(fit, n.ahead=1)$pred[1]
sigma.t1 <- predict(modelo.N.EVT , n.ahead=1)[1,1]

#Passo3: Estima o quantil com base na distribuição GEV
library(fExtremes)
sresi.menos <- -sresi
MLE <- gevFit( sresi.menos, type="pwm" )
xi <- MLE@fit$par.ests[1]
mu <- MLE@fit$par.ests[2]
sigma <- MLE@fit$par.ests[3]

#Passo4: Calcular o VaR
quantil <- qgev(lambda, xi = xi, mu = mu, beta = sigma, lower.tail = TRUE)
VaR.t1.N.EVT <- -(mu.t1 + (quantil[1]*sigma.t1) )



## metodo STEVT (Student´s T Extreme Value)
library(fGarch)
lambda <- 0.05

#Passo1: Encotrando os resíduos padronizados
fit <- arima( retorno, order=c(1,0,0) )
fit.res <- resid(fit)
modelo.S.EVT <- garchFit( ~garch(1, 1), 
                           fit.res, 
                           cond.dist = "sstd" )

sresi <- (modelo.S.EVT@residuals/modelo.S.EVT@sigma.t) 

#Passo2: Obtem-se sigma_t+1 e mu_t+1
mu.t1 <- predict(fit, n.ahead=1)$pred[1]
sigma.t1 <- predict(modelo.S.EVT, n.ahead=1)[1,1]

#Passo3: Estima o quantil com base na distribuição GEV
library(fExtremes)
sresi.menos <- -sresi
MLE <- gevFit( sresi.menos, type="pwm")
xi <- MLE@fit$par.ests[1]
mu <- MLE@fit$par.ests[2]
sigma <- MLE@fit$par.ests[3]

#Passo4: Calcular o VaR
quantil <- qgev(lambda, xi = xi, mu = mu, beta = sigma, lower.tail = TRUE)
VaR.t1.S.EVT <- -(mu.t1 + (quantil[1]*sigma.t1) )


# compara os metodos
data_frame( VaR.t1.N.FHS = VaR.t1.N.FHS * 100,
            VaR.t1.S.FHS = VaR.t1.S.FHS * 100,
            VaR.t1.N.EVT = VaR.t1.N.EVT * 100,
            VaR.t1.S.EVT = VaR.t1.S.EVT * 100 )


data_frame( VaR.t1.N.FHS = VaR.t1.N.FHS * 100,
            VaR.t1.S.FHS = VaR.t1.S.FHS * 100,
            VaR.t1.N.EVT = VaR.t1.N.EVT * 100,
            VaR.t1.S.EVT = VaR.t1.S.EVT * 100 ) %>% 
  gather( key = metodos, value = VaR ) %>% 
  ggplot( aes( x = metodos, y = VaR) ) +
  geom_bar( stat = "identity" )








