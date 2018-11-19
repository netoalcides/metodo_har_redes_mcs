######################################################################
#################### Exemplo Model Confidence Set ####################
######################################################################

#### Pacotes ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(doFuture)
library(rugarch)
library(MCS)

#### Exemplo 1 - utilizando o pacote rugarch ####

# dados
data(STOXXIndexesRet)

# dados de retorno da SXA1E
ret <- STOXXIndexesRet[,"SXA1E"]

# especifica o modelo
spec <- ugarchspec( mean.model = list( armaOrder = c(0, 0) ),
                    variance.model = list( model = "sGARCH", garchOrder = c(1, 1) ) )

# ajusta o modelo
fit <- ugarchfit(spec = spec, data = ret)

# previsao one-step ahead
OneStepForc <- ugarchforecast( fitORspec = fit, n.ahead = 1 )

# previsao com retreino
roll <- ugarchroll( spec = spec, 
                    data = ret, 
                    forecast.length = 2000,
                    refit.every = 5,
                    calculate.VaR = TRUE, 
                    VaR.alpha = 0.05 )

report( roll, 
        type="VaR", 
        VaR.alpha = 0.05,
        conf.level = 0.95 ) 

#### Using MCS ####

# models specification

models <- c("sGARCH")
distributions <- c("norm", "std", "ged")
spec.comp <- list()

for( m in models ) {
  
  for( d in distributions ) {
    
    spec.comp[[paste( m, d, sep = "-" )]] <-
      ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                 variance.model = list(model = m, garchOrder = c(1, 1)),
                 distribution.model=d)
    
  }
}

specifications <- names( spec.comp )


# Estima modelos
roll.comp <- list()
for( s in specifications ){
  
  roll.comp[[s]] <- ugarchroll(spec = spec.comp[[s]], data = ret,
                               forecast.length = 2000, refit.every = 200)
  
}

# obtem o VAR
VaR.comp=list()
for( s in specifications ) {
  VaR.comp[[s]] <- as.data.frame(roll.comp[[s]], which = "VaR")[, 1]
}

# obtem matriz LOSS
Loss <- do.call(cbind,lapply(specifications,
                             function(s) LossVaR(tau=0.01, realized=tail(ret, 2000)/100,
                                                 evaluated=VaR.comp[[s]]/100)))

colnames(Loss) <- specifications


loss <- Loss
data(Loss) # eh uma LOSS do proprio pacote

# usando o pacote MCS para fazer o mcs
SSM <- MCSprocedure(Loss = loss, alpha = 0.2, B = 5000, statistic = "Tmax")

# outra forma de fazer o mcs
SSM2 <- mcsTest( losses = as.matrix(loss), alpha = 0.2, nboot = 5000, nblock = 1, boot = "stationary" )
