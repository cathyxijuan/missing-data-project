
library(lavaan)

fitted.mod <- '     
f1 =~ NA*x1 + x2 + x3 +x4 + x5 + x6 
f1 ~~ 1*f1
'

pop.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6
f1 ~~ 0.5*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
'

6*7/2-6

n=1000000
percentMiss <- 0.5
data <- simulateData(pop.mod, sample.nobs = n, seed=123 )

fit <- cfa(fitted.mod, data=data, missing="fiml", mimic="EQS")
lavInspect( fit, "fit")[c("rmsea", "cfi")]


data[1:(n*percentMiss),1:3] <- NA
fit <- cfa(fitted.mod, data=data, missing="fiml", mimic="EQS")
lavInspect( fit, "fit")[c("rmsea", "cfi")]
