library(car)
library(plyr)
library(dplyr)
library(xts)
library(highfrequency)
library(nnet)
library(brnn)
library(timeDate)
require(robustbase)
library(forecast)
options("digits.secs"=9)

#Dir
rm(list=ls())
setwd("~/realized volatility")

####Help Functions

#Clean NA
comp.cases <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Create har flexible lag structure
#data - realized volatility
#structure - har lag structure: the original proposed by Corsi(2006) is 'structure = c(1, 5, 22)'
harstruc <- function(data, structure) {
  z <- lapply(structure, function(x){lag(rollmean(data, x, align = "right"), 1)})
  z2 <- do.call(cbind, z)
  names <- paste("rv", structure, sep = "")
  colnames(z2) <- names
  z2
}

#choose the number of hidden layers
mchoice <- function(train){

  m <- function(x){
    set.seed(5)
    sink('mod')
    mod <- brnn(bvrv~., data = train, neurons=x, normalize=FALSE, epochs=100, verbose=FALSE)
    sink()
    y <- predict(mod)
    rmse <- sqrt(mean((train[,1] - y)^2))
  }
  
  z <- unlist(lapply(c(1:10), m))
  which(unlist(z == min(z)), arr.ind = TRUE)
}

mchoice2 <- function(train){
  
  m <- function(x){
    set.seed(5)
    mod <- nnet(bvrv~., data = train, size=x, linout = T, maxit = 50,trace = F)
    y <- predict(mod)
    rmse <- sqrt(mean((train[,1] - y)^2))
  }
  
  z <- unlist(lapply(c(1:10), m))
  which(unlist(z == min(z)), arr.ind = TRUE)
}

#Plot forecast
plot_forecast <- function(original, forecast){
  
  rvpred <- merge(original, forecast)
  rvtest <- rvpred[,1]
  rvproj <- rvpred[,2]
  
  g_range = range(rvproj,rvtest)
  plot.zoo(rvtest,col="red",lwd=2, ylim=g_range,xlab="Tempo",ylab="Volatilidade Percebida"); 
  lines(rvproj,col="blue",lwd=2)
  legend("topright", c("Observado rRTSCov","Projetado rRTSCov"), cex=1.1, col=c("red","blue"),lty=1, lwd=2, bty="n")
}

#Equal Weighted combination method
equal <- function(listofmodels, train, test){
  l <- c(listofmodels)
  w <- 1/length(l)
  p <- lapply(l, function(x) {predict(get(x), test)})
  fs <- do.call(cbind, p)
  fs2 <- apply(fs, 1, function(x){sum(x*w)})
}

#Mean Squared Forecast Error combination method - MSFE
msfe <- function(listofmodels, train, test){
  l <- c(listofmodels)
  mse <- unlist(lapply(l, function(x) {accuracy(c(predict(get(x))), train[,1])[2]}))^2
  w <- (mse^-1)/(sum(mse^-1))
  p <- lapply(l, function(x) {predict(get(x), test)})
  fs <- do.call(cbind, p)
  fs2 <- apply(fs, 1, function(x){sum(x*w)})
}

#Lower Partial Moment Forecast Error combination method - LPMFE
lpmfe <- function(listofmodels, train, test){
  l <- c(listofmodels)
  e <- lapply(l, function(x) {c(predict(get(x)))-train[,1]})
  emin <- lapply(e, function(x) {pmin(x, 0)^2})
  emin <- do.call(cbind, emin)
  lpm <- apply(emin, 2, sum)
  w <- (lpm^-1)/(sum(lpm^-1))
  p <- lapply(l, function(x) {predict(get(x), test)})
  fs <- do.call(cbind, p)
  fs2 <- apply(fs, 1, function(x){sum(x*w)})
}

####Realized Volatility####
bvrv <- as.xts(read.zoo("bvrv.csv", header=FALSE, sep=";", dec = ".", FUN=as.POSIXct))
colnames(bvrv) <- "bvrv"
bvrv = bvrv[!is.na(bvrv)]*252; #Remove NA's
head(bvrv)
tail(bvrv)

###graph###
plot.zoo(bvrv,col="black",lwd=1,xlab="Tempo",ylab="Volatilidade Percebida")
#plot.zoo(bvrv,col="black",lwd=1,xlab="Time",ylab="Realized Volatility")

####Fixed window

##Original HAR##
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

har_original <- lm(bvrv~., data = rv_train)
summary(har_original)

#previsao
pred_har_original <- predict(har_original, rv_test)

##Optimized HAR##
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

#Treinamento
null <- lm(bvrv~1, data = rv_train)
full <- lm(bvrv~., data = rv_train)
har_opt <- step(null, scope = list(upper=full), direction="both",  k = log(n_train), trace = 0)
summary(har_opt)

#previsao
pred_har_opt <- predict(har_opt, rv_test)

##NNHAR original sem bayesian##
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

num.it <- 50 #number of training iterations
num.hid <- 10 #number of hidden layers

err.train <- matrix(nrow = num.it, ncol = num.hid)

for(i in 1:num.hid){
  for(j in 1:num.it){
    
    #to monitor progress
    cat(j,'\n') 
    flush.console()
    
    #to ensure same set of random starting weights are used each time
    set.seed(5)
    
    #temporary nnet model
    mod.tmp <- nnet(bvrv~., data = rv_train, size=i, linout = T, maxit = j,trace = F)
    
    #training and test errors
    train.err <- sqrt(mean(mod.tmp$residuals^2))
    
    #append error after each iteration
    err.train[j,i] <- train.err
  }
}

#plot error

plot(err.train[,1], type='l', ylab='RMSE',xlab='Iteration', col='red')
lines(err.train[,2],type='l', col='blue')
lines(err.train[,3],type='l', col='blue')
lines(err.train[,4],type='l', col='blue')
lines(err.train[,5],type='l', col='blue')
lines(err.train[,6],type='l', col='blue')
lines(err.train[,7],type='l', col='blue')
lines(err.train[,8],type='l', col='blue')
lines(err.train[,9],type='l', col='blue')
lines(err.train[,10],type='l', col='blue')

which(unlist(err.train[15,] == min(err.train[15,])), arr.ind = TRUE)

set.seed(5)
nnhar_original_sem_bayes <- nnet(bvrv~., data = rv_train, size=9, linout = T, maxit = 50,trace = F)
summary(nnhar_original_sem_bayes)

pred_nnhar_original_sem_bayes <- predict(nnhar_original_sem_bayes, rv_test)

##Optimized NNHAR sem bayesian##
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

num.it <- 50 #number of training iterations
num.hid <- 10 #number of hidden layers

err.train <- matrix(nrow = num.it, ncol = num.hid)

for(i in 1:num.hid){
  for(j in 1:num.it){
    
    #to monitor progress
    cat(j,'\n') 
    flush.console()
    
    #to ensure same set of random starting weights are used each time
    set.seed(5)
    
    #temporary nnet model
    mod.tmp <- nnet(bvrv~., data = rv_train, size=i, linout = T, maxit = j,trace = F)
    
    #training and test errors
    train.err <- sqrt(mean(mod.tmp$residuals^2))
    
    #append error after each iteration
    err.train[j,i] <- train.err
  }
}

#plot error

plot(err.train[,1], type='l', ylab='RMSE',xlab='Iteration', col='red')
lines(err.train[,2],type='l', col='blue')
lines(err.train[,3],type='l', col='blue')
lines(err.train[,4],type='l', col='blue')
lines(err.train[,5],type='l', col='blue')
lines(err.train[,6],type='l', col='blue')
lines(err.train[,7],type='l', col='blue')
lines(err.train[,8],type='l', col='blue')
lines(err.train[,9],type='l', col='blue')
lines(err.train[,10],type='l', col='blue')

which(unlist(err.train[20,] == min(err.train[20,])), arr.ind = TRUE)

set.seed(5)
nnhar_opt_sem_bayes <- nnet(bvrv~., data = rv_train, size=8, linout = T, maxit = 50,trace = F)
summary(nnhar_opt_sem_bayes)

pred_nnhar_opt_sem_bayes <- predict(nnhar_opt_sem_bayes, rv_test)

##NNHAR original##
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

#number of hidden layers choice
neurons <- mchoice(rv_train)
set.seed(5)
nnhar_original <- brnn(bvrv~., data = rv_train, neurons=neurons, normalize=FALSE, epochs=1000, verbose=FALSE)
summary(nnhar_original)
#previsao
pred_nnhar_original <- predict(nnhar_original, rv_test)

##Optimized NNHAR##
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
p <- 0.7
T <- dim(RV)[1]
n_train <- round(T*p)
rv_train <- RV[1:n_train,]
rv_test <- RV[(n_train+1):T,]

#number of hidden layers choice
neurons <- mchoice(rv_train)
set.seed(5)
nnhar_opt <- brnn(bvrv~., data = rv_train, neurons=neurons, normalize=FALSE, epochs=1000, verbose=FALSE)
summary(nnhar_opt)
#previsao
pred_nnhar_opt <- predict(nnhar_opt, rv_test)

##Ensemble##
models <- c("har_original", "har_opt", "nnhar_original_sem_bayes", "nnhar_opt_sem_bayes", "nnhar_original", "nnhar_opt")
pred_equal <- equal(models, rv_train, rv_test)
pred_msfe <- msfe(models, rv_train, rv_test)
pred_lpmfe <- lpmfe(models, rv_train, rv_test)


models <- c("har_opt", "nnhar_original_sem_bayes", "nnhar_original")
l <- c(models)
mse <- unlist(lapply(l, function(x) {accuracy(c(predict(get(x))), rv_train[,1])[2]}))^2
w <- (mse^-1)/(sum(mse^-1))

###Avaliação###
#Erro treino

rbind(accuracy(predict(har_original), rv_train[,1]), accuracy(predict(har_opt), rv_train[,1]),
      accuracy(predict(nnhar_original_sem_bayes)[,1], rv_train[,1]), accuracy(predict(nnhar_opt_sem_bayes)[,1], rv_train[,1]),
      accuracy(predict(nnhar_original), rv_train[,1]), accuracy(predict(nnhar_opt), rv_train[,1]))

#Erro Teste
rbind(accuracy(pred_har_original, rv_test[,1]), accuracy(pred_har_opt, rv_test[,1]),
      accuracy(c(pred_nnhar_original_sem_bayes), rv_test[,1]), accuracy(c(pred_nnhar_opt_sem_bayes), rv_test[,1]),
      accuracy(pred_nnhar_original, rv_test[,1]), accuracy(pred_nnhar_opt, rv_test[,1]),
      accuracy(pred_equal, rv_test[,1]), accuracy(pred_msfe, rv_test[,1]),
      accuracy(pred_lpmfe, rv_test[,1]))

###Grafico Projeção HAR###
plot_forecast(rv_test[,1], pred_har_original)
plot_forecast(rv_test[,1], pred_har_opt)
plot_forecast(rv_test[,1], pred_nnhar_original_sem_bayes)
plot_forecast(rv_test[,1], pred_nnhar_opt_sem_bayes)
plot_forecast(rv_test[,1], pred_nnhar_original)
plot_forecast(rv_test[,1], pred_nnhar_opt)
plot_forecast(rv_test[,1], pred_equal)
plot_forecast(rv_test[,1], pred_msfe)
plot_forecast(rv_test[,1], pred_lpmfe)


###Ensemble todas as combinações###
rmse_equal <- rmse_msfe <- rmse_lpmfe <- NULL
RMSE_equal <- vector('list', 5)
RMSE_msfe <- vector('list', 5)
RMSE_lpmfe <- vector('list', 5)
models <- c("har_original", "har_opt", "nnhar_original_sem_bayes", "nnhar_opt_sem_bayes", "nnhar_original", "nnhar_opt")
for (i in 2:6){
  
  md <- t(combn(models, i))
  
  for (j in 1:nrow(md)){
    
    pred_equal <- equal(unlist(md[j,]), rv_train, rv_test)
    rmse_equal[j] <- accuracy(pred_equal, rv_test[,1])[3]
    
    pred_msfe <- msfe(unlist(md[j,]), rv_train, rv_test)
    rmse_msfe[j] <- accuracy(pred_msfe, rv_test[,1])[3]
    
    pred_lpmfe <- lpmfe(unlist(md[j,]), rv_train, rv_test)
    rmse_lpmfe[j] <- accuracy(pred_lpmfe, rv_test[,1])[3]
  }
  
  #show(as.data.frame(cbind(md,rmse)))
  RMSE_equal[[i-1]] <- as.data.frame(cbind(md,rmse_equal))
  RMSE_msfe[[i-1]] <- as.data.frame(cbind(md,rmse_msfe))
  RMSE_lpmfe[[i-1]] <- as.data.frame(cbind(md,rmse_lpmfe))
}

#library(data.table)
#rbindlist(RMSE_equal, fill = TRUE)
#rbindlist(RMSE_msfe, fill = TRUE)
#rbindlist(RMSE_lpmfe, fill = TRUE)

####Recursive window

#Original HAR
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]

inicial_t <- 99
ehar_ori_train <- pred_ori_teste <- NULL
#e_ig <- e_er <- NULL
#rec.pred1 <- rec.pred2 <- NULL

for(i in 1:(T-inicial_t-1)){
  
  rv_train <- RV[1:(inicial_t+i),]
  rv_test <- RV[(inicial_t+1+i),]
  
  #Treinamento
  har_original <- lm(bvrv~., data = rv_train)
  
  #Erro treino
  ehar_ori_train[i] <- mean((rv_train[,1] - predict(har_original))^2)

  #Previsao teste
  pred_ori_teste[i] <- predict(har_original, rv_test)
}  

#Optimized Har
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]

inicial_t <- 99
ehar_opt_train <- pred_opt_teste <- NULL
#e_ig <- e_er <- NULL
#rec.pred1 <- rec.pred2 <- NULL

for(i in 1:(T-inicial_t-1)){
  
  rv_train <- RV[1:(inicial_t+i),]
  rv_test <- RV[(inicial_t+1+i),]
  
  #Treinamento
  null <- lm(bvrv~1, data = rv_train)
  full <- lm(bvrv~., data = rv_train)
  har_opt <- step(null, scope = list(upper=full), direction="both",  k = log(inicial_t+1), trace = 0)
  
  #Erro treino
  ehar_opt_train[i] <- mean((rv_train[,1] - predict(har_opt))^2)
 
  #Previsao teste
  pred_opt_teste[i] <- predict(har_opt, rv_test)
}  

#Original NNHAR
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]

inicial_t <- 200
erro_nnhar_original <- NULL
pred_nnhar_original <- NULL
#e_ig <- e_er <- NULL
#rec.pred1 <- rec.pred2 <- NULL

for(i in 1:(T-inicial_t-1)){
  
  rv_train <- RV[1:(inicial_t+i),]
  rv_test <- RV[(inicial_t+1+i),]
  
  #Treinamento
  neurons <- mchoice(rv_train)
  set.seed(5)
  nnhar_original <- brnn(bvrv~., data = rv_train, neurons=neurons, normalize=FALSE, epochs=50, verbose=FALSE)
  
  #Erro treino
  erro_nnhar_original[i] <- (accuracy(predict(nnhar_original), rv_train[,1])[2])^2
  
  #Previsao teste
  pred_nnhar_original[i] <- predict(nnhar_original, rv_test)
}  








#Erro treino
sqrt(mean(ehar_ori_train))
sqrt(mean(ehar_opt_train))

#Erro teste
accuracy(pred_ori_teste, RV[(inicial_t+1+1):T,1])
accuracy(pred_opt_teste, RV[(inicial_t+1+1):T,1])

####Rolling window

#Original HAR
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]

janela <- 1000
ehar_ori_train <- pred_ori_teste <- NULL
#e_ig <- e_er <- NULL
#rec.pred1 <- rec.pred2 <- NULL

for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  har_original <- lm(bvrv~., data = rv_train)
  
  #Erro treino
  ehar_ori_train[i] <- mean((rv_train[,1] - predict(har_original))^2)
  
  #Previsao teste
  pred_ori_teste[i] <- predict(har_original, rv_test)
  
  cat(i, '\n') 
  flush.console()
}  

#Optimized Har
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]

janela <- 1000
ehar_opt_train <- pred_opt_teste <- NULL
#e_ig <- e_er <- NULL
#rec.pred1 <- rec.pred2 <- NULL

for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  null <- lm(bvrv~1, data = data.frame(rv_train))
  full <- lm(bvrv~., data = data.frame(rv_train))
  har_opt <- step(null, scope = list(upper=full), direction="both",  k = log(janela+1), trace = 0)
  
  #Erro treino
  ehar_opt_train[i] <- mean((rv_train[,1] - predict(har_opt))^2)
  
  #Previsao teste
  pred_opt_teste[i] <- predict(har_opt, rv_test)
  
  cat(i, '\n') 
  flush.console()
}  

##NNHAR original##
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]
janela <- 1000
ennhar_original_train <- pred_nnhar_original_teste <- NULL


for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  #number of hidden layers choice
  #neurons <- mchoice(rv_train)
  set.seed(5)
  sink('nnhar_original')
  #nnhar_original <- brnn(bvrv~., data = rv_train, neurons=neurons, normalize=FALSE, epochs=100, verbose=FALSE)
  nnhar_original <- brnn(bvrv~., data = rv_train, neurons=10, normalize=FALSE, epochs=100, verbose=FALSE)
  sink()
  
  #Erro treino
  ennhar_original_train[i] <- mean((rv_train[,1] - predict(nnhar_original))^2)
  
  #Previsao teste
  pred_nnhar_original_teste[i] <- predict(nnhar_original, rv_test)

  #cat(i, neurons, '\n') 
  cat(i, '\n') 
  flush.console()
}  

##Optimized NNHAR##
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]
janela <- 1000
ennhar_opt_train <- pred_nnhar_opt_teste <- NULL

for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  #number of hidden layers choice
  #neurons <- mchoice(rv_train)
  set.seed(5)
  sink('nnhar_opt')
  #nnhar_opt <- brnn(bvrv~., data = rv_train, neurons=neurons, normalize=FALSE, epochs=100, verbose=FALSE)
  nnhar_opt <- brnn(bvrv~., data = rv_train, neurons=9, normalize=FALSE, epochs=100, verbose=FALSE)
  sink()
  
  #Erro treino
  ennhar_opt_train[i] <- mean((rv_train[,1] - predict(nnhar_opt))^2)
  
  #Previsao teste
  pred_nnhar_opt_teste[i] <- predict(nnhar_opt, rv_test)
  
  #to monitor progress
  #cat(i, neurons, '\n') 
  cat(i, '\n') 
  flush.console()
}  

##NNHAR original sem bayes##
l <- c(1, 5, 22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]
janela <- 1000
ennhar_original_sem_bayes_train <- pred_nnhar_original_sem_bayes_teste <- NULL

for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  #number of hidden layers choice
  #neurons <- mchoice2(rv_train)
  set.seed(5)
  #nnhar_original_sem_bayes <- nnet(bvrv~., data = rv_train, size=neurons, linout = T, maxit = 50,trace = F)
  nnhar_original_sem_bayes <- nnet(bvrv~., data = rv_train, size=9, linout = T, maxit = 50,trace = F)
  
  #Erro treino
  ennhar_original_sem_bayes_train[i] <- mean((rv_train[,1] - predict(nnhar_original_sem_bayes))^2)
  
  #Previsao teste
  pred_nnhar_original_sem_bayes_teste[i] <- predict(nnhar_original_sem_bayes, rv_test)
  
  #to monitor progress
  #cat(i, neurons, '\n') 
  cat(i, '\n') 
  flush.console()
}  

##Optimized NNHAR sem bayes##
l <- c(1:22)
RV <- comp.cases(merge(bvrv, harstruc(bvrv, l)))
T <- dim(RV)[1]
janela <- 1000
ennhar_opt_sem_bayes_train <- pred_nnhar_opt_sem_bayes_teste <- NULL

for(i in 1:(T-janela-1)){
  
  rv_train <- RV[i:(janela+i),]
  rv_test <- RV[(janela+1+i),]
  
  #Treinamento
  #number of hidden layers choice
  #neurons <- mchoice2(rv_train)
  set.seed(5)
  #nnhar_opt_sem_bayes <- nnet(bvrv~., data = rv_train, size=neurons, linout = T, maxit = 50,trace = F)
  nnhar_opt_sem_bayes <- nnet(bvrv~., data = rv_train, size=8, linout = T, maxit = 50,trace = F)
  
  
  #Erro treino
  ennhar_opt_sem_bayes_train[i] <- mean((rv_train[,1] - predict(nnhar_opt_sem_bayes))^2)
  
  #Previsao teste
  pred_nnhar_opt_sem_bayes_teste[i] <- predict(nnhar_opt_sem_bayes, rv_test)
  
  #to monitor progress
  #cat(i, neurons, '\n') 
  cat(i, '\n') 
  flush.console()
}  
############################################################################
####################### Combinations #######################

models <- c("har_original", "har_opt", "nnhar_original_sem_bayes", "nnhar_opt_sem_bayes", "nnhar_original", "nnhar_opt")
l_ori <- c(1, 5, 22)
l_opt <- c(1:22)
RV_ori <- comp.cases(merge(bvrv, harstruc(bvrv, l_ori)))
RV_opt <- comp.cases(merge(bvrv, harstruc(bvrv, l_opt)))

#Mean Squared Forecast Error combination method - MSFE
msfe <- function(listofmodels, valid, test){
  l <- c(listofmodels)
  mse <- unlist(lapply(l, function(x) {accuracy(c(predict(get(x), valid)), valid[,1])[2]}))^2
  w <- (mse^-1)/(sum(mse^-1))
  p <- lapply(l, function(x) {predict(get(x), test)})
  fs <- do.call(cbind, p)
  fs2 <- apply(fs, 1, function(x){sum(x*w)})
}

#Pesos MSFE
w_msfe <- function(listofmodels, valid, test){
  l <- c(listofmodels)
  mse <- unlist(lapply(l, function(x) {accuracy(c(predict(get(x), valid)), valid[,1])[2]}))^2
  w <- (mse^-1)/(sum(mse^-1))
}

#Lower Partial Moment Forecast Error combination method - LPMFE
lpmfe <- function(listofmodels, valid, test){
  l <- c(listofmodels)
  e <- lapply(l, function(x) {c(predict(get(x), valid)) - valid[,1]})
  emin <- lapply(e, function(x) {pmin(x, 0)^2})
  emin <- do.call(cbind, emin)
  lpm <- apply(emin, 2, sum)
  w <- (lpm^-1)/(sum(lpm^-1))
  p <- lapply(l, function(x) {predict(get(x), test)})
  fs <- do.call(cbind, p)
  fs2 <- apply(fs, 1, function(x){sum(x*w)})
}

#Pesos LPMFE
w_lpmfe <- function(listofmodels, valid, test){
  l <- c(listofmodels)
  e <- lapply(l, function(x) {c(predict(get(x), valid)) - valid[,1]})
  emin <- lapply(e, function(x) {pmin(x, 0)^2})
  emin <- do.call(cbind, emin)
  lpm <- apply(emin, 2, sum)
  w <- (lpm^-1)/(sum(lpm^-1))
}

T <- dim(RV_opt)[1]
janela <- 1000

Pesos_msfe <- list()
Pesos_lpmfe <- list()

prev_equal <- matrix(0, nrow = (T-janela-1), ncol=sum(unlist(lapply(2:6, function(x){choose(length(models),x)}))))
prev_msfe <- matrix(0, nrow = (T-janela-1), ncol=sum(unlist(lapply(2:6, function(x){choose(length(models),x)}))))
prev_lpmfe <- matrix(0, nrow = (T-janela-1), ncol=sum(unlist(lapply(2:6, function(x){choose(length(models),x)}))))

for(i in 1:(T-janela-1)){
  
  rv_train <- RV_ori[i:(janela+i),]
  rv_train_ori <- rv_train[1:round(dim(rv_train)[1]*0.7),]
  rv_valid_ori <- rv_train[(round(dim(rv_train)[1]*0.7)+1):dim(rv_train)[1],]
  rv_test_ori <- RV_ori[(janela+1+i),]
  
  rv_train <- RV_opt[i:(janela+i),]
  rv_train_opt <- rv_train[1:round(dim(rv_train)[1]*0.7),]
  rv_valid_opt <- rv_train[(round(dim(rv_train)[1]*0.7)+1):dim(rv_train)[1],]
  rv_test_opt <- RV_opt[(janela+1+i),]
  
  #Treinamento
  #HAR Original
  har_original <- lm(bvrv~., data = rv_train_ori)
  
  #HAR Optimized
  null <- lm(bvrv~1, data = data.frame(rv_train_opt))
  full <- lm(bvrv~., data = data.frame(rv_train_opt))
  har_opt <- step(null, scope = list(upper=full), direction="both",  k = log(janela+1), trace = 0)
  
  #NNHAR Original
  set.seed(5)
  sink('nnhar_original')
  nnhar_original <- brnn(bvrv~., data = rv_train_ori, neurons=10, normalize=FALSE, epochs=100, verbose=F)
  sink()
  
  #NNHAR Optimized
  set.seed(5)
  sink('nnhar_opt')
  nnhar_opt <- brnn(bvrv~., data = data.frame(rv_train_opt), neurons=9, normalize=FALSE, epochs=100, verbose=FALSE)
  sink()
  
  ##NNHAR original sem bayes##
  set.seed(5)
  nnhar_original_sem_bayes <- nnet(bvrv~., data = rv_train_ori, size=9, linout = T, maxit = 100,trace = F)
  
  ##NNHAR Optimized sem bayes##
  set.seed(5)
  nnhar_opt_sem_bayes <- nnet(bvrv~., data = rv_train_opt, size=8, linout = T, maxit = 100,trace = F)
  
  gc()
  
  #Combin Pesos Iguais
  Pred_equal <- list()
  for (k in 2:6){
    
    md <- t(combn(models, k))
    pred_equal <- lapply(1:nrow(md), function(j){equal(unlist(md[j,]), rv_train_opt, rv_test_opt)})
    pred_equal <- do.call(rbind, pred_equal)
    Pred_equal[[length(Pred_equal)+1]] <- pred_equal
    
  }
  prev_equal[i,] <- t(unlist(Pred_equal))  
  
  gc()
  
  ##Combin MSFE
  #Pesos
  W_msfe <- list()
  
  for (k in 2:6){
    
    md <- t(combn(models, k))
    w <- lapply(1:nrow(md), function(j){w_msfe(unlist(md[j,]), rv_valid_opt, rv_test_opt)})
    w <- do.call(rbind, w)
    W_msfe[[length(W_msfe)+1]] <- w
    
  }
  
  Pesos_msfe[[length(Pesos_msfe)+1]] <- ldply(W_msfe, rbind)
  
  #Previsoes
  Pred_msfe <- list()
  for (k in 2:6){
    
    md <- t(combn(models, k))
    pred_msfe <- lapply(1:nrow(md), function(j){msfe(unlist(md[j,]), rv_valid_opt, rv_test_opt)})
    pred_msfe <- do.call(rbind, pred_msfe)
    Pred_msfe[[length(Pred_msfe)+1]] <- pred_msfe
    
  }
  prev_msfe[i,] <- t(unlist(Pred_msfe))  
  
  ##Combin LPMFE
  #Pesos
  W_lpmfe <- list()
  
  for (k in 2:6){
    
    md <- t(combn(models, k))
    w <- lapply(1:nrow(md), function(j){w_lpmfe(unlist(md[j,]), rv_valid_opt, rv_test_opt)})
    w <- do.call(rbind, w)
    W_lpmfe[[length(W_lpmfe)+1]] <- w
    
  }
  
  Pesos_lpmfe[[length(Pesos_lpmfe)+1]] <- ldply(W_lpmfe, rbind)
  
  #Previsoes
  Pred_lpmfe <- list()
  for (k in 2:6){
    
    md <- t(combn(models, k))
    pred_lpmfe <- lapply(1:nrow(md), function(j){lpmfe(unlist(md[j,]), rv_valid_opt, rv_test_opt)})
    pred_lpmfe <- do.call(rbind, pred_lpmfe)
    Pred_lpmfe[[length(Pred_lpmfe)+1]] <- pred_lpmfe
    
  }
  prev_lpmfe[i,] <- t(unlist(Pred_lpmfe))  
  
  cat(i, '\n') 
  flush.console()
}  

##Modelos name
name <- list()

name <- lapply(2:6, function(k){
  md <- t(combn(models, k))
  ss <- lapply(1:nrow(md), function(j){paste(c(md[j,]), collapse = "_")})
  ss <- do.call(rbind, ss)
  name[[length(name)+1]] <- ss})

comb_modelos <- unlist(name)

##Modelos Pesos
#MSFE

Pesos_msfe <- lapply(Pesos_msfe, function(x){cbind(1:57,x)})
Pesos_msfe <- do.call(rbind, Pesos_msfe)
colnames(Pesos_msfe)[1] <- 'modelos'
Pesos_msfe$modelos <- as.factor(Pesos_msfe$modelos)

Pesos_msfe <- Pesos_msfe %>%
  group_by(modelos) %>%
  summarise_each(funs(mean))

Pesos_msfe <- as.data.frame(Pesos_msfe)
Pesos_msfe$modelos <- NULL
rownames(Pesos_msfe) <- comb_modelos

#LPMFE

Pesos_lpmfe <- lapply(Pesos_lpmfe, function(x){cbind(1:57,x)})
Pesos_lpmfe <- do.call(rbind, Pesos_lpmfe)
colnames(Pesos_lpmfe)[1] <- 'modelos'
Pesos_lpmfe$modelos <- as.factor(Pesos_lpmfe$modelos)

Pesos_lpmfe <- Pesos_lpmfe %>%
  group_by(modelos) %>%
  summarise_each(funs(mean))

Pesos_lpmfe <- as.data.frame(Pesos_lpmfe)
Pesos_lpmfe$modelos <- NULL
rownames(Pesos_lpmfe) <- comb_modelos

##Combinações list to dataframe
prev_equal <- data.frame(prev_equal)
colnames(prev_equal) <- comb_modelos

prev_msfe <- data.frame(prev_msfe)
colnames(prev_msfe) <- comb_modelos

prev_lpmfe <- data.frame(prev_lpmfe)
colnames(prev_lpmfe) <- comb_modelos


#save(pred_nnhar_opt_teste, file = "pred_nnhar_opt_teste.RData")
load('Pesos_msfe.RData')


#Erro treino
sqrt(mean(ehar_ori_train))
sqrt(mean(ehar_opt_train))
sqrt(mean(ennhar_original_train))
sqrt(mean(ennhar_opt_train))
sqrt(mean(ennhar_original_sem_bayes_train))
sqrt(mean(ennhar_opt_sem_bayes_train))

#Erro teste
accuracy(pred_ori_teste, RV[(janela+1+1):T,1])
accuracy(pred_opt_teste, RV[(janela+1+1):T,1])
accuracy(pred_nnhar_original_teste, RV[(janela+1+1):T,1])
accuracy(pred_nnhar_opt_teste, RV[(janela+1+1):T,1])
accuracy(pred_nnhar_original_sem_bayes_teste, RV[(janela+1+1):T,1])
accuracy(pred_nnhar_opt_sem_bayes_teste, RV[(janela+1+1):T,1])


############ Analises ############
rbind(
  accuracy(pred_ori_teste[884:1691], RV[1885:T,1]),
  accuracy(pred_opt_teste[884:1691], RV[1885:T,1]),
  accuracy(pred_nnhar_original_teste[884:1691], RV[1885:T,1]),
  accuracy(pred_nnhar_opt_teste[884:1691], RV[1885:T,1]),
  accuracy(pred_nnhar_original_sem_bayes_teste[884:1691], RV[1885:T,1]),
  accuracy(pred_nnhar_opt_sem_bayes_teste[884:1691], RV[1885:T,1])
)

equal_analysis <- lapply(1:57, function(j){accuracy(prev_equal[884:1691, j], RV[1885:T,1])})
msfe_analysis <- lapply(1:57, function(j){accuracy(prev_msfe[884:1691, j], RV[1885:T,1])})
lpmfe_analysis <- lapply(1:57, function(j){accuracy(prev_lpmfe[884:1691, j], RV[1885:T,1])})

equal_analysis <- data.frame(do.call(rbind, equal_analysis))
msfe_analysis <- data.frame(do.call(rbind, msfe_analysis))
lpmfe_analysis <- data.frame(do.call(rbind, lpmfe_analysis))

rownames(equal_analysis) <- colnames(prev_equal)
rownames(msfe_analysis) <- colnames(prev_msfe)
rownames(lpmfe_analysis) <- colnames(prev_lpmfe)

head(equal_analysis[order(equal_analysis$RMSE),])
head(msfe_analysis[order(msfe_analysis$RMSE),])
head(lpmfe_analysis[order(lpmfe_analysis$RMSE),])

head(equal_analysis[order(equal_analysis$MAE),])
head(msfe_analysis[order(msfe_analysis$MAE),])
head(lpmfe_analysis[order(lpmfe_analysis$MAE),])

plot_forecast(RV[(janela+1+1):T,1],pred_opt_teste)

#####Iterative chart#####
num.it <- 200 #number of training iterations
err.train <- NULL
err.test <- NULL

for(i in 1:num.it){
  
  #to ensure same set of random starting weights are used each time
  set.seed(5)
  
  #temporary nnet model
  mod.tmp <- brnn(bvrv~., data = nn_train, neurons=2, normalize=TRUE, epochs=i, verbose=FALSE)
  
  #training and test errors
  train.err <- sqrt(sum((predict(mod.tmp)-nn_train[,1])^2))
  test.pred <- predict(mod.tmp, newdata=nn_test)
  test.err <- sqrt(sum((test.pred-nn_test[,1])^2))
  
  #append error after each iteration
  err.train[i] <- train.err
  err.test[i] <- test.err
  
  plot(err.train[1:i], type='l', xlim=c(0,num.it), ylim=c(0,30), ylab='RMSE',xlab='Iteration', col='red')
  par(new=TRUE)    
  plot(err.test[1:i], type='l', xlim=c(0,num.it), ylim=c(0,30), ylab='RMSE',xlab='Iteration', col='blue')
}
