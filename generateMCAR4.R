library(lavaan)
library(simsem)



source("Models_CR2_3.R")

##FOR FOUR MISSING VARIABLES

#purpose: create missing data on x1,  x2, x3，x4.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMinPattern <- function(model, sample.nobs=1000000,  missing.percentage){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  simuData[1:(sample.nobs*missing.percentage),  c(9:12)] <-NA
  simuData
}





#Usage: only for this research.  Two variables has missing data; maximum number of missing patterns for two variables:x9, x10, x11 and x12
# Each variable with missing data has the given percentage of missing data. 
# It has four missing data patterns:1) missing data on x9 and x10; 
#                                   2) missing data on x11 and x12; 
#                                   3) missing data on x9, x10, x11 and x12; 
#                                   4) no missing data pattern

#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern2 <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  perc.mis.per.chunk <- missing.percentage/2 # 4 missing data patterns
  simuData[1:(sample.nobs*perc.mis.per.chunk), 9:10] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk+1):(sample.nobs*perc.mis.per.chunk*2),  11:12] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*2+1):(sample.nobs*perc.mis.per.chunk*3),  9:12] <-NA
  simuData
}






#Usage: only for this research.  Four variables has missing data; max number of missing patterns for four variables: x9, x10, x11 and x12
# Each variable with missing data has the given percentage of missing data. 
# It has four missing data patterns:1) missing data on x9; 
#                                   2) missing data on x10; 
#                                   3) missing data on x11; 
#                                   4) missing data on x12; 
#                                   5) missing data on x9 and x10; 
#                                   6) missing data on x9 and x11; 
#                                   7) missing data on x9 and x12; 
#                                   8) missing data on x10 and x11; 
#                                   9) missing data on x10 and x12; 
#                                   10) missing data on x11 and x12; 
#                                   11) missing data on x9, x10 and x11; 
#                                   12) missing data on x9, x10 and x12; 
#                                   13) missing data on x9, x11 and x12; 
#                                   14) missing data on x10, x11 and x12; 
#                                   15) missing data on x9, x10, x11 and x12
#                                   16) no missing data pattern

#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  num.missing.var <- 4
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  perc.mis.per.chunk <- missing.percentage/(2^num.missing.var/2) # 16 missing data patterns
  simuData[1:(sample.nobs*perc.mis.per.chunk),  9] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk+1):(sample.nobs*perc.mis.per.chunk*2),  10] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*2+1):(sample.nobs*perc.mis.per.chunk*3),  11] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*3+1):(sample.nobs*perc.mis.per.chunk*4),  12] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*4+1):(sample.nobs*perc.mis.per.chunk*5),  c(9,10)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*5+1):(sample.nobs*perc.mis.per.chunk*6),  c(9,11)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*6+1):(sample.nobs*perc.mis.per.chunk*7), c(9,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*7+1):(sample.nobs*perc.mis.per.chunk*8),  c(10,11)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*8+1):(sample.nobs*perc.mis.per.chunk*9),  c(10,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*9+1):(sample.nobs*perc.mis.per.chunk*10),  c(11,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*10+1):(sample.nobs*perc.mis.per.chunk*11),  c(9,10,11)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*11+1):(sample.nobs*perc.mis.per.chunk*12), c(9,10,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*12+1):(sample.nobs*perc.mis.per.chunk*13),  c(9,11,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*13+1):(sample.nobs*perc.mis.per.chunk*14), c(10,11,12)] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*14+1):(sample.nobs*perc.mis.per.chunk*15), 9:12] <-NA
  simuData
}










#Usage: put fit indices for a list of models into a matrix

#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
#missing.type: a character: "min", "max", "max2". "max1"=each variable has the same proportion of missingness but the total proportion is different. 
############ "max2" = the total proportion of missingness is the same but the each variable has different proportions of missingness.
fit.ind.matrix.MCAR <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 6)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else if (missing.type == "max"){
      simuData <- MCARMaxPattern(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {
      simuData <- MCARMaxPattern2(pop.model.list[[i]], sample.nobs, missing.percentage)
    }
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df")])
  }
  
  fit.indices.MCAR
  
}


#Usage: put sigma.hat for a list of models into a list

#Arguments: same as above

sigma.hat.MCAR <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  sigma.hat <-list()
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else if (missing.type == "max"){
      simuData <- MCARMaxPattern(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {
      simuData <- MCARMaxPattern2(pop.model.list[[i]], sample.nobs, missing.percentage)
    }
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    sigma.hat[[i]]<- lavInspect(fit, "cov.ov")
  }
  
  sigma.hat
  
}



sigmaHat_MCAR_MinPat_20PerMiss_4VarMiss_CR2_3 <- 
   sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")
sigmaHat_MCAR_MinPat_50PerMiss_4VarMiss_CR2_3 <- 
   sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")
# fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")
# fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")
# round(fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_3,6) 
# round(fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3,6)
# 
save(sigmaHat_MCAR_MinPat_20PerMiss_4VarMiss_CR2_3, file="sigmaHat_MCAR_MinPat_20PerMiss_4VarMiss_CR2_3.RData")
save(sigmaHat_MCAR_MinPat_50PerMiss_4VarMiss_CR2_3, file="sigmaHat_MCAR_MinPat_50PerMiss_4VarMiss_CR2_3.RData")
# save(fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_3, file="fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_3.RData")
# save(fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3, file="fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3.RData")



sigmaHat_MCAR_MaxPat_20PerMiss_4VarMiss_CR2_3 <- 
  sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")
sigmaHat_MCAR_MaxPat_50PerMiss_4VarMiss_CR2_3 <- 
  sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")
# fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")
# fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")
# round(fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_3,6) 
# round(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3,6)

save(sigmaHat_MCAR_MaxPat_20PerMiss_4VarMiss_CR2_3, file="sigmaHat_MCAR_MaxPat_20PerMiss_4VarMiss_CR2_3.RData")
save(sigmaHat_MCAR_MaxPat_50PerMiss_4VarMiss_CR2_3, file="sigmaHat_MCAR_MaxPat_50PerMiss_4VarMiss_CR2_3.RData")
# save(fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_3, file="fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_3.RData")
# save(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3, file="fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3.RData")



0.4469/0.077