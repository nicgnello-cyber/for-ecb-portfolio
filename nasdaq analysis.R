library(quantmod)

nasdaq <- getSymbols("^NDX", from="2020-01-01", to="2025-01-01", auto.assign=FALSE)
nasdaq <- nasdaq[,6] #prendiamo l'adjusted close (gli altri valori non ci interessano)
y <- as.ts(nasdaq)
head(y)

plot(y, main= ' Adjusted Close ')

acf(y, 400)  #non stazionarietà, decadimento dell'acf molto lento.
             #forte memoria del passato, sembra una rw

pacf(y,400)


ly = log(y)
ts.plot(ly)

# log returns  

ry = diff(ly) * 100
ts.plot(ry)
acf(ry)   #now it's stationary

plot(ry, main='Returns')
abline(h = mean(ry, na.rm = TRUE), col = "red")  
#returns act like WN with mean around 0
#there is clearly volatility clustering


mean(ry)  #positive expected value
var(ry)

hist(ry,breaks=100,col='red',density=50) #heavy tail, the assumption of normality would not be correct
qqnorm(y = scale(ry))                    #same as above
abline(a = 0, b = 1, col = "red")


#The returns are clearly not iid ( to be expected in finance ) and not normal,
#for this reason we can expect the garch model to be not appropriate

#####
## GARCH

n <- length(ry)
parametri_garch <- c(0.01, 0.1, 0.95)
lower <- c(1e-8, 0, 0)
upper <- c(Inf,  10, 0.999)


garch_topt <- function(parametri, dati=ry){
  
  n <- length(dati)
  
  omega <- parametri[1]
  alfa <- parametri[2]
  beta <- parametri[3]
  
  v = rep(NA, n)
  sigma = rep(NA, n)
  lambda = rep(NA, n)
  
  lambda[1] <- var(dati)
  v[1] <- (dati[1])^2 -lambda[1]
  sigma[1] <- sqrt(lambda[1])
  
  for(t in 2:n){
    lambda[t] = omega + beta*lambda[t-1] + alfa*(dati[t-1]^2)
    v[t] = (dati[t])^2 -lambda[t]
    sigma[t] = sqrt(lambda[t])
  }
  
  likelihood <- -0.5 * sum(log(2 * pi) + log(lambda) + (dati^2) / lambda)
  
  return(-likelihood)
}



ottimizzatore_garch<-nlminb(start = parametri_garch, objective = garch_topt, 
                             dati  = ry, gradient = NULL, 
                             control = list(trace = 0), hessian = NULL,
                             lower = lower, upper = upper)

garch<- function(parametri, dati=ry){
  
  n <- length(dati)
  
  omega <- parametri[1]
  alfa <- parametri[2]
  beta <- parametri[3]
  
  v = rep(NA, n)
  sigma = rep(NA, n)
  lambda = rep(NA, n)
  
  lambda[1] <- var(dati)
  v[1] <- (dati[1])^2 -lambda[1]
  sigma[1] <- sqrt(lambda[1])
  
  for(t in 2:n){
    lambda[t] = omega + beta*lambda[t-1] + alfa*(dati[t-1]^2)
    v[t] = (dati[t])^2 -lambda[t]
    sigma[t] = sqrt(lambda[t])
  }
  likelihood <- -0.5 * sum(log(2 * pi) + log(lambda) + (dati^2) / lambda)
  return(list(sigma = sigma, likelihood = likelihood, score = v))
}  



risultati_garch <- garch(dati = ry, parametri = ottimizzatore_garch$par)


sigma_garch <- risultati_garch$sigma
score_garch <- risultati_garch$score

AIC_garch <- -2 * risultati_garch$likelihood + 2 * length(parametri_garch)
BIC_garch <- -2 * risultati_garch$likelihood + log(n) * length(parametri_garch)

acf(score_garch)
pacf(score_garch)
hist(score_garch)





#####
## BETA-T-GARCH

n <- length(ry)
parametri_t_garch <- c(0.01, 0.1, 0.95, 3)
lower <- c(1e-8, 0, 0, 2.01)
upper <- c(Inf,  10, 0.999, Inf)


beta_t_garch_topt <- function(parametri, dati=ry){
  
  n <- length(dati)
  
  omega <- parametri[1]
  alfa <- parametri[2]
  beta <- parametri[3]
  v <- parametri[4]
  
  u = rep(NA, n)
  sigma = rep(NA, n)
  lambda = rep(NA, n)
  
  lambda[1] <- var(dati)
  
  u[1] <- lambda[1] * (((v+1)*dati[1]^2 / ((v-2)*lambda[1] + dati[1]^2)) - 1)
  sigma[1] <- sqrt(lambda[1])
  
  for(t in 2:n){
    lambda[t] = omega + beta*lambda[t-1] + alfa*u[t-1]
    u[t] = lambda[t] * (((v+1)*dati[t]^2 / ((v-2)*lambda[t] + dati[t]^2)) - 1)
    sigma[t] = sqrt(lambda[t])
  }
  
  likelihood <- sum(lgamma((v+1)/2) - lgamma(v/2) - 0.5*log(v*pi) - log(sigma) - ((v+1)/2)*log(1+(dati^2)/(sigma^2 *v)))
  
  return(-likelihood)
}



ottimizzatore_tgarch<-nlminb(start = parametri_t_garch, objective = beta_t_garch_topt, 
                             dati  = ry, gradient = NULL, 
                             control = list(trace = 0), hessian = NULL,
                             lower = lower, upper = upper)

beta_t_garch <- function(dati=ry, parametri){
  
  n <- length(dati)
  omega <- parametri[1]
  alfa <- parametri[2]
  beta <- parametri[3]
  v <- parametri[4]
  
  u = rep(NA, n)
  sigma = rep(NA, n)
  lambda = rep(NA, n)
  
  lambda[1] <- var(dati)
  u[1] <- lambda[1] * (((v+1)*dati[1]^2 / ((v-2)*lambda[1] + dati[1]^2)) - 1)
  sigma[1] <- sqrt(lambda[1])
  
  for(t in 2:n){
    lambda[t] = omega + beta*lambda[t-1] + alfa*u[t-1]
    u[t] = lambda[t] * (((v+1)*dati[t]^2 / ((v-2)*lambda[t] + dati[t]^2)) - 1)
    sigma[t] = sqrt(lambda[t])
  }
  
  likelihood <- sum(lgamma((v+1)/2) - lgamma(v/2) - 0.5*log(v*pi) - log(sigma) - ((v+1)/2)*log(1+(dati^2)/(sigma^2 *v)))
  
  return(list(sigma = sigma, likelihood = likelihood, score = u))
}


risultati_tgarch <- beta_t_garch(dati = ry, parametri = ottimizzatore_tgarch$par)


sigma_tgarch <- risultati_tgarch$sigma
score_tgarch <- risultati_tgarch$score

AIC_tgarch <- -2 * risultati_tgarch$likelihood + 2 * length(parametri_t_garch)
BIC_tgarch <- -2 * risultati_tgarch$likelihood + log(n) * length(parametri_t_garch)

acf(score_tgarch)
pacf(score_tgarch)
hist(score_tgarch)

#####
#BETA-T-EGARCH
n<-length(ry)
parametri_egarch<-c(0.01, 0.1, 0.95, 3)
lower <- c(-Inf, 0, -0.999, 2.0001)
upper <- c(Inf, 10, 0.999, Inf)

beta_t_egarch_topt<-function(parametri,dati=ry){
  
  n<-length(dati)
  
  omega<-parametri[1]
  alfa<-parametri[2]
  beta<-parametri[3]
  v<-parametri[4]
  
  u=rep(NA,n)
  sigma<-rep(NA,n)
  lambda<-rep(NA,n)
  
  lambda[1]<-log(var(dati))
  u[1]<-((v+1)*dati[1]^2/((v-2)*exp(lambda[1])+dati[1]^2))-1
  sigma[1]<-exp(lambda[1]/2)
  for(t in 2:n){
    
    lambda[t] = omega + beta*lambda[t-1] + alfa*u[t-1]
    u[t]=((v+1)*dati[t]^2/((v-2)*exp(lambda[t])+dati[t]^2))-1
    sigma[t]=exp(lambda[t]/2)
    
  }
  
  likelihood<-sum(lgamma((v+1)/2)-lgamma(v/2)-0.5*log(v*pi)-log(sigma)-((v+1)/2)*log(1+(dati^2)/(sigma^2 *v)))
  

  return(-likelihood)
  
}



ottimizzatore_egarch<-nlminb(start = parametri_egarch, objective = beta_t_egarch_topt, 
                             dati  = ry, gradient = NULL, 
                             control = list(trace = 0), hessian = NULL,
                             lower = lower, upper = upper)
#we search best parameter value with l-bfgs-b algo


beta_t_egarch<-function(dati=ry,parametri){

  omega<-parametri[1]
  alfa<-parametri[2]
  beta<-parametri[3]
  v<-parametri[4]
  u=rep(NA,n)
  sigma<-rep(NA,n)
  lambda<-rep(NA,n)

  lambda[1]<-log(var(dati))
  u[1]<-((v+1)*dati[1]^2/((v-2)*exp(lambda[1])+dati[1]^2))-1
  sigma[1]<-exp(lambda[1]/2)
  for(t in 2:n){
  
    lambda[t] = omega + beta*lambda[t-1] + alfa*u[t-1]
    u[t]=((v+1)*dati[t]^2/((v-2)*exp(lambda[t])+dati[t]^2))-1
    sigma[t]=exp(lambda[t]/2)
  
  }
  
  likelihood<-sum(lgamma((v+1)/2)-lgamma(v/2)-0.5*log(v*pi)-log(sigma)-((v+1)/2)*log(1+(dati^2)/(sigma^2 *v)))
  
  return(list(sigma = sigma, likelihood = likelihood, score=u))
  
}

risultati_egarch<-beta_t_egarch(parametri = ottimizzatore_egarch$par)
sigma_egarch<-risultati_egarch$sigma  #sigma
score_egarch<-risultati_egarch$score


AIC_egarch<- (-2*beta_t_egarch(parametri = ottimizzatore_egarch$par)$likelihood)+2*length(parametri_egarch)
BIC_egarch<- (-2*beta_t_egarch(parametri = ottimizzatore_egarch$par)$likelihood)+log(n)*length(parametri_egarch)


acf(score_egarch)
pacf(score_egarch)
hist(score_egarch)






#####

index=50   #burn-in period
ry_index=ry[index:n]
time_index<-1:length(ry_index)

#GARCH

sigma_index1<-sigma_garch[index:n]

plot(ry_index,col='grey',type='l')
lines(sigma_index1,col='red')


plot(abs(ry_index),col='grey',type='l')
lines(sigma_index1,col='red')

plot((ry_index)^2,col='grey',type='l')
lines(sigma_index1,col='red')

#BETA-T-GARCH

sigma_index2<-sigma_tgarch[index:n]

plot(ry_index,col='grey',type='l')
lines(sigma_index2,col='red')

plot(abs(ry_index),col='grey',type='l')
lines(sigma_index2,col='red')

plot((ry_index)^2,col='grey',type='l')
lines(sigma_index2,col='red')



#BETA-T-EGARCH

sigma_index3<-sigma_egarch[index:n]

plot(ry_index,col='grey',type='l')
lines(sigma_index3,col='red')


plot(abs(ry_index),col='grey',type='l')
lines(sigma_index3,col='red')

plot((ry_index)^2,col='grey',type='l')
lines(sigma_index3,col='red')


#comparison estimates of 3 models

plot(abs(ry_index),col='grey',type='l',
     main = 'Confronto Volatilità Condizionata vs Rendimenti Assoluti',
     ylim = c(0, max(abs(ry_index))))
lines(sigma_index1, col = 'black')
lines(sigma_index2, col= 'blue')
lines(sigma_index3, col= 'red')
legend("topright",
       legend=c('|RETURNS|',"GARCH","BETA-T-GARCH","BETA-T-EGARCH"),
       col=c('grey','black','blue','red'), lty = 1,
       cex=0.8)

#comparison of scores

d_garch  <- density(score_garch, na.rm = TRUE)
d_tgarch <- density(score_tgarch, na.rm = TRUE)
d_egarch <- density(score_egarch, na.rm = TRUE)


max_y <- max(d_garch$y, d_tgarch$y, d_egarch$y)
plot(d_garch, col = 'black', lwd = 2, 
     main = "Score density comparison",
     xlab = "Score", ylab = "Density",
     ylim = c(0, max_y)) 
lines(d_tgarch, col = 'blue', lwd = 2)
lines(d_egarch, col = 'red', lwd = 2)
legend("topright", 
       legend = c("GARCH", "BETA-T-GARCH", "BETA-T-EGARCH"),
       col = c('black', 'blue', 'red'),
       lty = 1, lwd = 2, cex = 0.8)
#garch si spalma su tutta l'asse, questo perchè assumendo normalità non funziona bene
#quando lo usiamo per dati finanziari che non sono normali.
#il garch non ha un picco particolarmente alto attorno allo 0.


#beta-t-garch e beta-t-egarch hanno un picco molto alto attorno allo 0, con code 
#che si annullano quasi subito ( in particolare il beta-t-egarch ).
#

# Analisi out-of-sample ####

n<-length(ry)
cut<-floor(0.8*n)
train<-ry[1:cut]
test<-ry[(length(train)+1):n]



ottimizzatore_garch_train <- nlminb(start = parametri_garch, objective = garch_topt, 
                                    dati = train, control = list(trace = 0), 
                                    lower = lower, upper = c(Inf, 10, 0.999))
risultati_garch_train <- garch(dati = ry, parametri = ottimizzatore_garch_train$par)
sigma_prev_test_garch <- risultati_garch_train$sigma[(length(train)+1):n]



ottimizzatore_tgarch_train <- nlminb(start = parametri_t_garch, objective = beta_t_garch_topt, 
                                     dati = train, control = list(trace = 0), 
                                     lower = c(1e-8, 0, 0, 2.01), upper = c(Inf, 10, 0.999, Inf))
risultati_tgarch_train <- beta_t_garch(dati = ry, parametri = ottimizzatore_tgarch_train$par) 
sigma_prev_test_tgarch <- risultati_tgarch_train$sigma[(length(train)+1):n]


ottimizzatore_tegarch_train <- nlminb(start = parametri_egarch, objective = beta_t_egarch_topt, 
                                      dati = train, control = list(trace = 0), 
                                      lower = c(-Inf, 0, -0.999, 2.0001), upper = c(Inf, 10, 0.999, Inf))
risultati_tegarch_train <- beta_t_egarch(dati = ry, parametri = ottimizzatore_tegarch_train$par)
sigma_prev_test_tegarch <- risultati_tegarch_train$sigma[(length(train)+1):n]


mse_garch <- mean((test^2 - sigma_prev_test_garch^2)^2)
mse_beta_t_garch<-mean((test^2 - sigma_prev_test_tgarch^2)^2)
mse_beta_t_egarch<-mean((test^2 - sigma_prev_test_tegarch^2)^2)

print(mse_garch)
print(mse_beta_t_garch)
print(mse_beta_t_egarch)



calcola_qlike <- function(var_reale, var_prevista) {
  eps <- 1e-10
  var_prevista <- pmax(var_prevista, eps)
  var_reale <- pmax(var_reale, eps)
  
  rapporto <- var_reale / var_prevista
  qlike_t <- rapporto - log(rapporto) - 1
  return(mean(qlike_t))
}


qlike_garch <- calcola_qlike(test^2,sigma_prev_test_garch^2)
qlike_t_garch<-calcola_qlike(test^2,sigma_prev_test_tgarch^2)
qlike_t_egarch<-calcola_qlike(test^2,sigma_prev_test_tegarch^2)

print(qlike_garch)
print(qlike_t_garch)
print(qlike_t_egarch)





plot(((length(train)+1):n), abs(test), type = 'l', col = 'grey', 
     main = "Previsioni Out-of-Sample (1-step ahead) vs Rendimenti Assoluti",
     ylab = "Volatilità / |Rendimenti|", xlab = "Tempo (Indice Test Set)",
     ylim = c(0, max(abs(test))))
lines(((length(train)+1):n), sigma_prev_test_garch, col = 'black', lwd = 2)
lines(((length(train)+1):n),sigma_prev_test_tgarch,col = 'blue',lwd=2)
lines(((length(train)+1):n),sigma_prev_test_tegarch,col = 'red',lwd=2)
legend("topright", legend = c("|Rendimenti Test|", "Previsioni GARCH","Previsioni TGARCH","Previsioni T-EGARCH"),
       col = c("grey", "black","blue",'red'), lty = 1, lwd = c(1, 2), cex = 0.8)