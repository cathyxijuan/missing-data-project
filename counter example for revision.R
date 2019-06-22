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
x1 ~ 0.7*1
x2 ~ 0.7*1
x3 ~ 0.7*1
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

#generate a complete dataset 
n=100000
simuData <- simulateData(pop.mod, sample.nobs = n, seed=250)
cov(simuData)
summary(simuData)

#create MAR missing values 
cutoff <- qnorm(0.3) #the cutoff point is -0.5244005. The cutoff is just picked at random.
simuData[simuData[,2] >cutoff,1] <-NA 
missing.proportion <- sum(is.na(simuData[,1]))/n #This cutoff resulted in 0.88923 proportion of missing data 
missing.proportion

#missing the model
fit <- cfa(fitted.mod, data=simuData, mimic="EQS",  missing="fiml")

#EM/saturated model mean
lavInspect(fit, "sampstat.h1")


#Pattern specific means (truncated means)
#Pattern1: cases with complete data
simu_comp <- simuData[is.na(simuData[,1]) ==F,]
mu_truncated1 <- sapply(simu_comp, mean)
mu_truncated1

#Pattern 2: cases with incomplete data
simu_missing <- simuData[is.na(simuData[,1]),2:6]
mu_truncated2 <- sapply(simu_missing, mean)
mu_truncated2
#adding a place holder for the missing variable 
mu_truncated2.dummy <- c(0,mu_truncated2 )
mu_truncated2.dummy


#comparing the weighted pattern specific means with the EM means. 
missing.proportion*mu_truncated2.dummy+(1-missing.proportion)*mu_truncated1
lavInspect(fit, "sampstat.h1")$mean


fitted.mod.sat <- ' 
x1 ~ 1
x2 ~ 1
x3 ~ 1
x4 ~ 1
x5 ~ 1
x6 ~ 1
x1 ~~ x1+x2 +x3+x4+x5+x6
x2 ~~ x2+x3+x4+x5+x6
x3 ~~ x3+x4+x5+x6
x4 ~~ x4+x5+x6
x5 ~~ x5+x6
x6 ~~ x6'



fit.sat <- cfa(fitted.mod.sat, data=simuData, mimic="EQS",  missing="fiml")  #this generates error
fitted(fit.sat)
lavInspect(fit, "sampstat.h1")
