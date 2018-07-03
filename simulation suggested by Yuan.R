library(lavaan)
library(simsem)
library(tmvtnorm)




pop.mod <- '     
x1~~1*x1
x2~~ 2*x2
x1~~0.5*x2
'


fitted.mod1<- '     
x1~~a*x1
x2~~a*x2
x1~~b*x2
'

fitted.mod2<- '     
x1~~a*x1
x2~~b*x2
x1~~0*x2
'

fitted.mod3<- '     
x1~~a*x1
x2~~1*x2
x1~~b*x2
'
fitted.mod4<- '     
x1~~.5*x1
x2~~a*x2
x1~~b*x2
'

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=191)
sigma<- cov(simuData)
fit1<- cfa(fitted.mod1, data=simuData, missing="fiml", mimic="EQS")
fit2<- cfa(fitted.mod2, data=simuData, missing="fiml", mimic="EQS")
fit3<- cfa(fitted.mod3, data=simuData, missing="fiml", mimic="EQS")
fit4<- cfa(fitted.mod4, data=simuData, missing="fiml", mimic="EQS")

ccov1 <- lavInspect(fit1, "cov.ov")
ccov2 <- lavInspect(fit2, "cov.ov")
ccov3 <- lavInspect(fit3, "cov.ov")
ccov4 <- lavInspect(fit4, "cov.ov")

fit1
fit2
fit3
fit4
ccov1
ccov2
ccov3
ccov4


simuData[1:(sample.nobs*missing.percentage),  2] <-NA

ifit1<- cfa(fitted.mod1, data=simuData, missing="fiml", mimic="EQS")
ifit2<- cfa(fitted.mod2, data=simuData, missing="fiml", mimic="EQS")
ifit3<- cfa(fitted.mod3, data=simuData, missing="fiml", mimic="EQS")
ifit4<- cfa(fitted.mod4, data=simuData, missing="fiml", mimic="EQS")

icov1 <- lavInspect(ifit1, "cov.ov")
icov2 <- lavInspect(ifit2, "cov.ov")
icov3 <- lavInspect(ifit3, "cov.ov")
icov4 <- lavInspect(ifit4, "cov.ov")
lavInspect(ifit4, "fit")
lavInspect(fit4, "fit")


icov1
ccov1
icov2
ccov2
icov3
ccov3
icov4
ccov4

fit1

ifit1

fit2
ifit2

fit3
ifit3

fit4
ifit4

lavInspect(fit1, "fit")["fmin"]
lavInspect(ifit1, "fit")["fmin"]

lavInspect(fit2, "fit")["fmin"]
lavInspect(ifit2, "fit")["fmin"]

lavInspect(fit3, "fit")["fmin"]
lavInspect(ifit3, "fit")["fmin"]

lavInspect(fit4, "fit")["fmin"]
lavInspect(ifit4, "fit")["fmin"]


##looking at model 4
sigma <- sigma*(nrow(simuData)-1)/nrow(simuData)
sigmahat <-ccov4*(nrow(simuData)-1)/nrow(simuData)
fmin.complete <- log(det(sigmahat))+sum(diag(sigma%*%solve(sigmahat)))-log(det(sigma))-2
fmin.complete/2

fmin.incomplete <- 0.5*fmin.complete + 0.5*(log(0.5)+1/0.5-log(1)-1)
fmin.incomplete/2





# in bollen's book GOOD EXAMPLE FOR PLACE OF MISFIT AND PLACE OF MISSING ARE the same

pop.mod <- '   
y~~ 1*y
x~~2*x
x~~0.5*y
'

fit.mod <- "
y~1*x"

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")
names(simuData)
simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")
#the x variable actually fits perfectly. So after deleting the data on y, the misfit is deleted. This kind of problem can be solved by two-stage



# in bollen's book
pop.mod <- '     
y~~ 2.5*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~1*x"

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")
names(simuData)
simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



# in bollen's book
pop.mod <- ' 
y~~ 2*y
x~~ 2*x
x~~0*y
'

fit.mod <- "
y~1*x
x~~0*y"

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")["cfi"]
names(simuData)
simuData[1:(sample.nobs*missing.percentage),  1] <-NA


fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



#A good example in which the fit doesn't change. !!!!!!!!!!!!!!!!!!!

# in bollen's book
pop.mod <- '     
y~~ 4*y
x~~2*x
x~~1*y
'

fit.mod <- "
y~1*x
x~~0.1*x"

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



# NULL MODEL FITTING IMPORTANT FOR CFI
pop.mod <- '     
y~~ 2*y
x~~1*x
x~~0.5*y
'

fit.mod <- "
y~~0*x
y~~y
x~~x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")




# NULL MODEL FITTING IMPORTANT FOR CFI
pop.mod <- '     
y~~ 2*y
x~~1*x
x~~0.5*y
'

fit.mod <- "
y~~0*x
y~~y
x~~x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")




#THE FIT DOESNT CHANGE!!!! AN EXAMPLE OF THE FIT DOESN'T CHANGE. NOTE THIS IS NOT THE NULL MODEL. In null model, we don't fix the var
#In this case, we fixed all the parameters; so we can calculate Fmin right away. No need to take derivatives
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~~0*x
y~~1*y
x~~.5*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.2
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")




#Opposite of the above; THIS CAN ALSO BE AN EXAMPLE OF WHERE MISSING AND MISFIT ARE AT THE SAME PLACE. 
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~~0*x
y~~.5*y
x~~2*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.2
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



#Misfit on the connection. Missing any variable will always result in better fit
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~~0.5*x
y~~1*y
x~~2*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.2
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")
0.077/2




#trying saturated model under MCAR and MAR
pop.mod <- '     
y~~ 2*y
x~~1*x
x~~0.5*y
'

fit.mod <- "
y~~x
x~~x
y~~y"



sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

#MCAR
simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")


#MAR
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
simuData[simuData[,2] <0,1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")






#trying saturated model under MCAR and MAR
pop.mod <- '     
y~~ 2*y
x~~1*x
x~~0.5*y
'

fit.mod <- "
y~1*x"



sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

#MCAR
simuData[1:(sample.nobs*missing.percentage),  1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")


#MAR
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
simuData[simuData[,2] <0,1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")


#Investigating MAR

#Opposite of the above; THIS CAN ALSO BE AN EXAMPLE OF WHERE MISSING AND MISFIT ARE AT THE SAME PLACE. 
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~~0*x
y~~.5*y
x~~2*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA  #MAR AND MCAR GAVE THE SAME RESULTS Í
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
simuData[simuData[,2] <0,1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



##Missing on the connection
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0*y
'

fit.mod <- "
y~~.5*x
y~~1*y
x~~2*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA  #MAR AND MCAR GAVE THE SAME RESULTS Í
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
simuData[simuData[,2] <0,1] <-NA

fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")




#Misfit on the connection. compare MCAR and MAR
pop.mod <- '     
y~~ 1*y
x~~2*x
x~~0.5*y
'

fit.mod <- "
y~~0*x
y~~1*y
x~~2*x"

sample.nobs=1000000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
sigma<- cov(simuData)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")

simuData[1:(sample.nobs*missing.percentage),  1] <-NA


fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")


simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)
simuData[simuData[,2] >0,1] <-NA
cov(simuData[complete.cases(simuData),])
fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit") #fmin=0.013
lavInspect(fitmis, "sampstat.h1")
lavInspect(fitmis, "mean.ov")

#calculating Fmin from just matrices
sigma <- matrix(c(1,0.5, 0.5, 2), nrow=2)
sigma.hat <- matrix(c(1,0, 0, 2), nrow=2)
#Pattern 1 cov matrix; for complete data
mu1    <- rep(0,2)
lower <- rep(-Inf,2)
upper <- c(Inf, 0)
mu_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tmean
sigma_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tvar


#Pattern 2 cov matrix
sigma_t <- sigma[-1,-1]
mu2    <- 0
lower <- 0
upper <- Inf
mu_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tmean
sigma_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tvar


#caculate MAR fmin
p=2
sigma_hat1 <- sigma.hat
sigma_hat2 <- sigma.hat[2:p, 2:p]
sigma1 <- sigma
sigma2 <- sigma[2:p, 2:p]



mu_hat1 <- c(mu_truc1[1],  0.000) 
mu_hat2 <-  0

p11 <- log(det(sigma_hat1%*%solve(sigma1)))

p12 <- sum(diag((sigma_truc1 + (mu_truc1-mu_hat1)%*%t(mu_truc1-mu_hat1))%*%solve(sigma_hat1)))

p13 <- sum(diag((sigma_truc1 + (mu_truc1-mu1)%*%t(mu_truc1-mu1))%*%solve(sigma1)))

F_p1 <- p11+p12-p13
F_p1
p21 <- log(det(sigma_hat2%*%solve(sigma2)))

p22 <- sum(diag((sigma_truc2 + (mu_truc2-mu_hat2)%*%t(mu_truc2-mu_hat2))%*%solve(sigma_hat2)))

p23 <- sum(diag((sigma_truc2 + (mu_truc2-mu2)%*%t(mu_truc2-mu2))%*%solve(sigma2)))

F_p2 <- p21+p22-p23
F_p2

F_pop <- 0.5*(F_p1+F_p2)
F_pop/2
