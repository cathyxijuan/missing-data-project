library(lavaan)

#DP= misfit and missing are in same place 

fitted.mod <- '     
f1 =~ NA*x1 + x2 + x3 +x4 + x5 + x6
f2 =~ NA*x7 + x8 + x9 +x10 + x11 + x12
f1 ~~ 1*f1
f2 ~~ 1*f2'



pop.mod1 <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6
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
x7 ~~ .51*x7
x8 ~~ .51*x8
x9 ~~ .51*x9
x10 ~~ .51*x10
x11 ~~ .51*x11
x12 ~~ .51*x12
'



##place of misfit and place of missing are different; 0 factor correlation 

pop.mod2.1 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.1*x2
'

pop.mod2.2 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.2*x2
'

pop.mod2.3 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.3*x2
'

pop.mod2.4 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.4*x2
'




pop.mod2.5 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.5*x2
'



##place of misfit and place of missing are different; 0.2 factor correlation 


pop.mod3.1 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.2*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.1*x2
'

pop.mod3.2<- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.2*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.2*x2
'

pop.mod3.3 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.2*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.3*x2
'

pop.mod3.4 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.2*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.4*x2
'




pop.mod3.5 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.2*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.5*x2
'

##place of misfit and place of missing are different; 0.4 factor correlation 


pop.mod4.1 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.1*x2
'

pop.mod4.2 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.2*x2
'

pop.mod4.3 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.3*x2
'

pop.mod4.4 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.4*x2
'




pop.mod4.5 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.5*x2
'

##place of misfit and place of missing are different; 0.6 factor correlation 


pop.mod5.1 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.6*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.1*x2
'

pop.mod5.2 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.6*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.2*x2
'

pop.mod5.3 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.6*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.3*x2
'

pop.mod5.4 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.6*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.4*x2
'




pop.mod5.5 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
f2 =~ .7*x7 + .7*x8 + .7*x9 +.7*x10 + .7*x11 + .7*x12 
f1 ~~ 0.6*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x1 ~~ 0.5*x2
'





pop.mod <- list( pop.mod1, 
                 pop.mod2.1, pop.mod2.2, pop.mod2.3, pop.mod2.4, pop.mod2.5,
                 pop.mod3.1, pop.mod3.2, pop.mod3.3, pop.mod3.4, pop.mod3.5,
                 pop.mod4.1, pop.mod4.2, pop.mod4.3, pop.mod4.4, pop.mod4.5,
                 pop.mod5.1, pop.mod5.2, pop.mod5.3, pop.mod5.4, pop.mod5.5)
# 
sigma <-list()
 for(i in 1:length(pop.mod)){
   fit <-cfa(pop.mod[[i]], data=simulateData(pop.mod[[i]], sample.nobs=1000))
  sigma[[i]]<- lavInspect(fit, "cov.ov")
 }
# 
sigma.hat <-list()
#  
  for(i in 1:length(pop.mod)){
#    
  fit <- cfa(model=fitted.mod, sample.nobs=300, sample.cov=sigma[[i]], mimic="EQS") 
    sigma.hat[[i]]<- lavInspect(fit, "cov.ov")
  }
# # 

fit.indices.comp <-matrix( nrow = 0, ncol = 6)
# 
for(i in 1:length(pop.mod)){
     simuData <- simulateData(pop.mod[[i]], sample.nobs=1000000,seed=111)
     fit <- cfa(fitted.mod, data=simuData)
     fit.indices.comp<- rbind(fit.indices.comp,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df")])
   }
round(fit.indices.comp,4)
fit.complete.CR <- fit.indices.comp
# 
 save(fit.complete.CR, file="fit.complete.CR_SP.RData")
save(sigma.hat, file="sigma.hat.CR_SP.RData")
save(sigma, file="sigma.CR_SP.RData")

