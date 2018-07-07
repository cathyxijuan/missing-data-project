library(lavaan)
source("functions.R")
library(tmvtnorm)


#misfit and missing are in different places; different factors

fitted.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6'



## 0 factor correlation
pop.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x1 ~~0.4*x2
'


fit <-cfa(pop.mod, data=simulateData(pop.mod, sample.nobs=1000))

sigma<- lavInspect(fit, "cov.ov")
sigma


fit1 <- cfa(fitted.mod, data=simulateData(fitted.mod, sample.nobs=1000))
sigma.hat <- lavInspect(fit1, "cov.ov")
sigma.hat




#####
n=1000000
missing.percentage=0.5
simuData <- simulateData(pop.mod, sample.nobs = n)
# no missing 
fit.f <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")
summary(fit.f)
lavInspect(fit.f, "fit")[c("fmin", "cfi","rmsea", "df")]


#same place ; missing
simuData <- simulateData(pop.mod, sample.nobs = n)
simuData[1:n*missing.percentage, 1:2]  <- NA
fit.miss <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")
lavInspect(fit.miss, "fit")[c("fmin", "cfi","rmsea")]


#Different place; missing
simuData1 <- simulateData(pop.mod, sample.nobs = n)
simuData1[1:n*missing.percentage, 6]  <- NA
fit.miss2 <- cfa(fitted.mod, data=simuData1, mimic="EQS",  missing="fiml")
lavInspect(fit.miss2, "fit")[c("cfi","rmsea")]



#complete; Calculating by hand 
p=6
F_1 <- log(det(sigma.hat%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.hat)))-p
F_1/2



#Same place; calculating by hand 
p_1 = 6
F_1 <- log(det(sigma.hat%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.hat)))-p


p_2 = 5
sigma2 <- sigma[2:6, 2:6]
sigma.hat2 <- sigma.hat[2:6,2:6]
F_2 <- log(det(sigma.hat2%*%solve(sigma2))) + sum(diag(sigma2%*%solve(sigma.hat2)))-p_2

(F_1+F_2)/2




#Different place; calculating by hand 
p_1 = 6
F_1 <- log(det(sigma.hat%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.hat)))-p


p_2 = 5
sigma2 <- sigma[1:5, 1:5]
sigma.hat2 <- sigma.hat[1:5,1:5]
F_2 <- log(det(sigma.hat2%*%solve(sigma2))) + sum(diag(sigma2%*%solve(sigma.hat2)))-p_2

(F_1+F_2)/2






###Connection 
fitted.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6'



## missing on connection
pop.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0.8*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
'


fit <-cfa(pop.mod, data=simulateData(pop.mod, sample.nobs=1000))

sigma<- lavInspect(fit, "cov.ov")
sigma


fit1 <- cfa(fitted.mod, data=simulateData(fitted.mod, sample.nobs=1000))
sigma.hat <- lavInspect(fit1, "cov.ov")
sigma.hat


#####
n=1000000
simuData <- simulateData(pop.mod, sample.nobs = n)
missing.percentage=0.5
# no missing 
fit.f <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")
summary(fit.f)
lavInspect(fit.f, "fit")[c("fmin", "cfi","rmsea", "df")]


#missing
simuData <- simulateData(pop.mod, sample.nobs = n)
simuData[1:n*missing.percentage, 1]  <- NA
fit.miss <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")
lavInspect(fit.miss, "fit")[c("fmin", "cfi","rmsea", "df")]

p_1 = 6
F_1 <- log(det(sigma.hat%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.hat)))-p


p_2 = 5
sigma2 <- sigma[2:6, 2:6]
sigma.hat2 <- sigma.hat[2:6,2:6]
F_2 <- log(det(sigma.hat2%*%solve(sigma2))) + sum(diag(sigma2%*%solve(sigma.hat2)))-p_2

(F_1+F_2)/2


















#MAR
library(lavaan)
source("functions.R")
library(tmvtnorm)


#misfit and missing are in different places; different factors

fitted.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1'



## 0 factor correlation
pop.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x1 ~~0.4*x2
'


fit <-cfa(pop.mod, data=simulateData(pop.mod, sample.nobs=1000))

sigma<- lavInspect(fit, "cov.ov")
sigma


fit1 <- cfa(fitted.mod, data=simulateData(fitted.mod, sample.nobs=1000))
sigma.hat <- lavInspect(fit1, "cov.ov")
sigma.hat

















n=1000000
simuData3 <- simulateData(pop.mod, sample.nobs = n)
simuData3[simuData3[,2] >0,1] <-NA #delete the x1 when x2 is greater than 0
fit.miss3 <- cfa(fitted.mod, data=simuData3, mimic="EQS",  missing="fiml")
lavInspect(fit.miss3, "fit")[c("fmin","cfi","rmsea", "df")]
lavInspect(fit.miss3, "mean.ov")



#Calculcating MAR by hand 
#Pattern1 : complete
mu1    <- rep(0,6)
lower <- rep(-Inf,6)
upper <- c(Inf, 0, rep(Inf, 4))
mu_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tmean
sigma_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tvar


#Pattern2 : x1 is missing
sigma_t <- sigma[-1,-1]
mu2    <- rep(0, 5)
lower <- c(0, rep(-Inf, 4))
upper <- rep(Inf,5)
mu_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tmean
sigma_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tvar



#caculate MAR fmin
p=6
sigma_hat1 <- sigma.hat
sigma_hat2 <- sigma.hat[2:p, 2:p]
sigma1 <- sigma
sigma2 <- sigma[2:p, 2:p]


mu_hat1 <- c(0, rep(0,5))
mu_hat2 <-  mu_hat1[2:p]


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

6*7/2
3/sqrt(20)
