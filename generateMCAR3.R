library(lavaan)
library(simsem)

source("Models_CR2_3.R")




#purpose: create missing data on x11,  x12.
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
  simuData[1:(sample.nobs*missing.percentage),  c(11:12)] <-NA
  simuData
}



#Usage: only for this research.  Two variables has missing data; maximum number of missing patterns for two variables: x11 and x12
       # Each variable with missing data has the given percentage of missing data. 
       # It has four missing data patterns:1) missing data on x11; 2) missing data on x12; 3) missing data on x11 and x12; 4) no missing data pattern
     
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  perc.mis.per.chunk <- missing.percentage/2 # 4 missing data patterns
  simuData[1:(sample.nobs*perc.mis.per.chunk), 11] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk+1):(sample.nobs*perc.mis.per.chunk*2),  12] <-NA
  simuData[(sample.nobs*perc.mis.per.chunk*2+1):(sample.nobs*perc.mis.per.chunk*3), 11:12] <-NA
  simuData
}





#Usage: only for this research. Two variables have missing data; maximum number of missing patterns for two variables: (1) and (2) 
       #percentage of missing follows a staircase shape (i.e., the percentage of missing in one or more variables is the giving percentage)
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern2 <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  simuData <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  for(i in 0:1){
    simuData[1:(sample.nobs*missing.percentage/(2^i)),11+i] <- NA
  }
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



# sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR2_3 <- sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")
# sigmaHat_MCAR_MinPat_50PerMiss_2VarMiss_CR2_3 <- sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")
# fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_3 <- fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")
# fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_3 <- fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")
# round(fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_3,6) 
# round(fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_3,6)


# save(sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR2_3, file="sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR2_3.RData")
# save(sigmaHat_MCAR_MinPat_50PerMiss_2VarMiss_CR2_3, file="sigmaHat_MCAR_MinPat_50PerMiss_2VarMiss_CR2_3.RData")
# save(fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_3, file="fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_3.RData")
# save(fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_3, file="fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_3.RData")







sigmaHat_MCAR_MaxPat_20PerMiss_2VarMiss_CR2_3 <- 
  sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

sigmaHat_MCAR_MaxPat_50PerMiss_2VarMiss_CR2_3 <- 
  sigma.hat.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")

fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_3 <- 
  fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_3 <- 
  fit.ind.matrix.MCAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")


round(fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_3,6) 

round(fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_3,6)



save(sigmaHat_MCAR_MaxPat_20PerMiss_2VarMiss_CR2_3, file="sigmaHat_MCAR_MaxPat_20PerMiss_2VarMiss_CR2_3.RData")
save(sigmaHat_MCAR_MaxPat_50PerMiss_2VarMiss_CR2_3, file="sigmaHat_MCAR_MaxPat_50PerMiss_2VarMiss_CR2_3.RData")
save(fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_3, file="fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_3.RData")
save(fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_3, file="fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_3.RData")














 