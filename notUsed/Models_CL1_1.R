library(lavaan)

#DP= misfit and missing are in same place 

fitted.mod <- '     
f1 =~ NA*x1 + x2 + x3 +x4 + x5 + x6
f2 =~ NA*x7 + x8 + x9 +x10 + x11 + x12
f1 ~~ 1*f1
f2 ~~ 1*f2'

#Factor correlation is 0.


pop.mod1.1 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.1*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.5*x12 #1-0.7^2-0.1^2
'


pop.mod1.2 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.2*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.47*x12 #1-0.7^2-0.2^2
'



pop.mod1.3 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.3*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.42*x12 #1-0.7^2-0.3^2
'










#Factor correlation is 0.4.


pop.mod2.1 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.1*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.4*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.444*x12 #1-0.7^2-0.1^2-2*0.7*0.4*0.1
'




pop.mod2.2 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.2*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.4*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.358*x12 #1-0.7^2-0.2^2-2*0.7*0.4*0.2
'


pop.mod2.3 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.3*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.4*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.252*x12 #1-0.7^2-0.3^2-2*0.7*0.4*0.3
'








#Factor correlation is 0.8.

pop.mod3.1 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.1*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.8*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7   
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.388*x12 #1-0.7^2-0.1^2-2*0.7*0.1*0.8
'





pop.mod3.2 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.2*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.8*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7 
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.246*x12     #1-0.7^2-0.2^2-2*0.7*0.2*0.8
'

pop.mod3.3 <- '     ##population model 
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 + 0.3*x12
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.8*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ 0.51*x7 
x8 ~~  0.51*x8
x9 ~~  0.51*x9
x10 ~~  0.51*x10
x11 ~~  0.51*x11
x12 ~~  0.084*x12     #1-0.7^2-0.3^2-2*0.7*0.3*0.8
'















pop.mod <- list( pop.mod1.1, pop.mod1.2, pop.mod1.3,
                 pop.mod2.1, pop.mod2.2, pop.mod2.3,
                 pop.mod3.1, pop.mod3.2, pop.mod3.3)






sigma <-list()

for(i in 1:length(pop.mod)){
   fit <-cfa(pop.mod[[i]], data=simulateData(pop.mod[[i]], sample.nobs=1000))
  sigma[[i]]<- lavInspect(fit, "cov.ov")
 }


sigma.hat <-list()

for(i in 1:length(pop.mod)){
  fit <- cfa(model=fitted.mod, sample.nobs=300, sample.cov=sigma[[i]], mimic="EQS")
   sigma.hat[[i]]<- lavInspect(fit, "cov.ov")
}


fit.indices.comp <-matrix( nrow = 0, ncol = 6)

for(i in 1:length(pop.mod)){
     simuData <- simulateData(pop.mod[[i]], sample.nobs=1000000,seed=111)
      fit <- cfa(fitted.mod, data=simuData)
     fit.indices.comp<- rbind(fit.indices.comp,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df")])
}
round(fit.indices.comp,4)
fitNoMissing_CL1_1 <- fit.indices.comp
sigmaHat_CL1_1 <- sigma.hat
sigma_CL1_1 <- sigma


save(fitNoMissing_CL1_1, file="fitNoMissing_CL1_1.RData")
save(sigmaHat_CL1_1, file="sigmaHat_CL1_1.RData")
save(sigma_CL1_1, file="sigma_CL1_1.RData")
