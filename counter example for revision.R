library(lavaan)
source("functions.R")
library(tmvtnorm)


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
x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1
x4 ~ 0.7*1
x5 ~ 0.7*1
x6 ~ 0.7*1'


fitted.mod <- '     
f1 =~ NA*x1 + x2 + x3 
f2 =~ NA*x4 + x5 + x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1
x4 ~ 0*1
x5 ~ 0*1
x6 ~ 0*1'

n=1000000
simuData <- simulateData(pop.mod, sample.nobs = n, seed=250)
cov(simuData)
summary(simuData)

#create MAR data 
cutoff <- qnorm(0.8)
simuData[simuData[,2] >cutoff,1] <-NA 

fit <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")
lavInspect(fit, "mean.ov")

