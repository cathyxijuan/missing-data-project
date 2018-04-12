library(lavaan)
source("functions.R")

#misfit and missing are in different place and same place

fitted.mod <- '     
f1 =~ NA*x1 + x2 + x3 +x4 + x5 + x6
f2 =~ NA*x7 + x8 + x9 +x10 + x11 + x12
f1 ~~ 1*f1
f2 ~~ 1*f2'


## 0 factor correlation 

pop.mod1.0 <- '     
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




pop.mod1.1 <- '    
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
x7 ~~ 0.1*x8
x9 ~~ 0.1*x10
'

pop.mod1.2 <- '    
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
x7 ~~ 0.2*x8
x9 ~~ 0.2*x10
'

pop.mod1.3 <- '    
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
x7 ~~ 0.3*x8
x9 ~~ 0.3*x10
'

pop.mod1.4 <- '    
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
x7 ~~ 0.4*x8
x9 ~~ 0.4*x10
'








## 0.4 factor correlation 
pop.mod2.0 <- '    
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
'

pop.mod2.1 <- '    
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
x9 ~~ 0.1*x10
x7 ~~ 0.1*x8
'

pop.mod2.2 <- '    
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
x7 ~~ 0.2*x8
x9 ~~ 0.2*x10
'

pop.mod2.3 <- '    
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
x7 ~~ 0.3*x8
x9 ~~ 0.3*x10
'

pop.mod2.4 <- '    
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
x7 ~~ 0.4*x8
x9 ~~ 0.4*x10
'





##0.8 factor correlation 
pop.mod3.0 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
'


pop.mod3.1 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x7 ~~ 0.1*x8
x9 ~~ 0.1*x10
'

pop.mod3.2 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x7 ~~ 0.2*x8
x9 ~~ 0.2*x10
'

pop.mod3.3 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x7 ~~ 0.3*x8
x9 ~~ 0.3*x10
'

pop.mod3.4 <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 +.7*x4 + .7*x5 + .7*x6 
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
x7 ~~ .51*x7
x8 ~~  .51*x8
x9 ~~  .51*x9
x10 ~~  .51*x10
x11 ~~  .51*x11
x12 ~~ .51*x12
x7 ~~ 0.4*x8
x9 ~~ 0.4*x10
'






pop.mod <- list( pop.mod1.0, pop.mod1.1, pop.mod1.2, pop.mod1.3, pop.mod1.4,
                 pop.mod2.0, pop.mod2.1, pop.mod2.2, pop.mod2.3, pop.mod2.4,
                 pop.mod3.0, pop.mod3.1, pop.mod3.2, pop.mod3.3, pop.mod3.4)

# sigma <-list()
# for(i in 1:length(pop.mod)){
#   fit <-cfa(pop.mod[[i]], data=simulateData(pop.mod[[i]], sample.nobs=1000))
#   sigma[[i]]<- lavInspect(fit, "cov.ov")
# }
# 
# sigma.hat <-list()
# 
# for(i in 1:length(pop.mod)){
# 
#   fit <- cfa(model=fitted.mod, sample.nobs=300, sample.cov=sigma[[i]], mimic="EQS")
#   sigma.hat[[i]]<- lavInspect(fit, "cov.ov")
# }
# 
# sigmaHat_CR2_2 <- sigma.hat
# sigma_CR2_2 <-sigma
# 
# save(sigmaHat_CR2_2, file="sigmaHat_CR2_2.RData")
# save(sigma_CR2_2, file="sigma_CR2_2.RData")
# 
# 
# 
# fit.indices.comp <-matrix( nrow = 0, ncol = 12)
# 
# for(i in 1:length(pop.mod)){
#   simuData <- simulateData(pop.mod[[i]], sample.nobs=1000000,seed=111)
#   fit <- cfa(fitted.mod, data=simuData)
#   fit.indices.comp<- rbind(fit.indices.comp,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", "chisq", "pvalue", "baseline.chisq", "baseline.df", "rmsea.ci.lower", "rmsea.ci.upper")])
# }
# 
# 
# 
# fitNoMissing_CR2_2  <- fit.indices.comp
# rmsea_NoMissing_CR2_2 <- rmsea_table(fitNoMissing_CR2_2)
# cfi_NoMissing_CR2_2 <- cfi_table(fitNoMissing_CR2_2)
# fitNoMissingShort_CR2_2 <- list(rmsea=rmsea_NoMissing_CR2_2, cfi=cfi_NoMissing_CR2_2 )
# fitNoMissingShort_CR2_2 
# 
# save(fitNoMissing_CR2_2 , file="fitNoMissing_CR2_2.RData")
# save(fitNoMissingShort_CR2_2 , file="fitNoMissingShort_CR2_2.RData")