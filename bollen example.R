library(lavaan)
library(simsem)
library(tmvtnorm)



# in bollen's book
pop.mod <- ' 
y~~ .5*y
x~~ .5*x
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
sigma <- matrix(c(0.5, 0, 0 , 0.5), nrow=2)
sigmaHat <- matrix(c(1.5, 0.5, 0.5 , 0.5), nrow=2)
a <- log(det(sigmaHat%*%solve(sigma))) +sum(diag(sigma%*%solve(sigmaHat)))-2
a/2
a/4

log(2)/2




# in bollen's book
pop.mod <- ' 
y~~ .5*y
x~~ .5*x
x~~0*y
'

fit.mod <- "
y~1*x
x~~.5*x"

sample.nobs=1000000
missing.percentage=0.5
simuData<- simulateData(pop.mod , sample.nobs=sample.nobs,seed=111)

fit<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fit)
lavInspect(fit, "cov.ov")
lavInspect(fit, "fit")
names(simuData)
simuData[1:(sample.nobs*missing.percentage),  2] <-NA




fitmis<- sem(fit.mod, data=simuData, missing="fiml", mimic="EQS")
summary(fitmis)
lavInspect(fitmis, "cov.ov")
lavInspect(fitmis, "fit")



1.15-0.415
0.5+1/3
1.238 -0.5
1.5-1/3
1+sqrt(1-2*(0.735-1)/0.735)
1-sqrt(1-2*(0.735-1)/0.735)
a<-0.738
0.738^3*2-(3/4)*0.738-0.25
-0.5*(a-1)*(a+0.5)^2
0.5*a^3
0.933-0.2

fac <- c(1, 1/2, 1/4,1/8)
fac2 <- c(fac,-fac)
func <- 8*fac2^3-3*fac2-1
func

func <- 2*fac2^3-3/4*fac2-1/4
func

d <- 0.7378355
f=0.5*(log(2*d)+1/d-1)+0.5*(log(2*d+1)+1/(2*d+1)-1)
f/2
