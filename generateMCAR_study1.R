library(lavaan)
source("functions.R")
source("Models_2CR_SF.R")
##MISSING IS ON x11 and x12 or x9-12.


### FOR TWO MISSING VARIABLES
#purpose: create missing data on x11,  x12.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCAR_2Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
  simuData[ind,11:12] <- NA
  simuData
}










#Usage: put fit indices for a list of models into a matrix

#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
fit.ind.matrix.MCAR_2Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000, 
                                     missing.percentage){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  for(i in 1:length(pop.model.list)){
    simuData <- MCAR_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", 
                                                                       "baseline.df","rmsea.ci.lower", "rmsea.ci.upper")])
    }
    fit.indices.MCAR
}



##FOR FOUR MISSING VARIABLES

#purpose: create missing data on x1,  x2, x3，x4.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCAR_4Var <- function(model, sample.nobs=1000000,  missing.percentage){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
  simuData[ind,9:12] <- NA
  simuData
}








#Usage: put fit indices for a list of models into a matrix

#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
fit.ind.matrix.MCAR_4Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000, 
                                     missing.percentage){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  for(i in 1:length(pop.model.list)){
  simuData <- MCAR_4Var(pop.model.list[[i]], sample.nobs, missing.percentage)
  fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
  fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", "baseline.df",
                                                                       "rmsea.ci.lower", "rmsea.ci.upper")])
  }
  fit.indices.MCAR
  
}







fitMCAR_20PerMiss_2VarMiss_2CR_SF <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, 
                           missing.percentage = 0.20)

fitMCAR_50PerMiss_2VarMiss_2CR_SF <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, 
                           missing.percentage = 0.50)


fitMCAR_20PerMiss_4VarMiss_2CR_SF <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, 
                           missing.percentage = 0.20)

fitMCAR_50PerMiss_4VarMiss_2CR_SF <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, 
                           missing.percentage = 0.50)



fitMCAR_2CR_SF <- list(I20PerMiss_2VarMiss=fitMCAR_20PerMiss_2VarMiss_2CR_SF, 
                      I50PerMiss_2VarMiss=fitMCAR_50PerMiss_2VarMiss_2CR_SF, 
                      I20PerMiss_4VarMiss=fitMCAR_20PerMiss_4VarMiss_2CR_SF, 
                      I50PerMiss_4VarMiss=fitMCAR_50PerMiss_4VarMiss_2CR_SF) 

fitMCAR_2CR_SF 



rmsea_MCAR_20PerMiss_2VarMiss_2CR_SF = rmsea_table(fitMCAR_20PerMiss_2VarMiss_2CR_SF)
rmsea_MCAR_50PerMiss_2VarMiss_2CR_SF = rmsea_table(fitMCAR_50PerMiss_2VarMiss_2CR_SF)
rmsea_MCAR_20PerMiss_4VarMiss_2CR_SF = rmsea_table(fitMCAR_20PerMiss_4VarMiss_2CR_SF) 
rmsea_MCAR_50PerMiss_4VarMiss_2CR_SF = rmsea_table(fitMCAR_50PerMiss_4VarMiss_2CR_SF) 
cfi_MCAR_20PerMiss_2VarMiss_2CR_SF = cfi_table(fitMCAR_20PerMiss_2VarMiss_2CR_SF)
cfi_MCAR_50PerMiss_2VarMiss_2CR_SF = cfi_table(fitMCAR_50PerMiss_2VarMiss_2CR_SF)
cfi_MCAR_20PerMiss_4VarMiss_2CR_SF = cfi_table(fitMCAR_20PerMiss_4VarMiss_2CR_SF) 
cfi_MCAR_50PerMiss_4VarMiss_2CR_SF = cfi_table(fitMCAR_50PerMiss_4VarMiss_2CR_SF) 



fitMCAR_Short_2CR_SF <- list(rmsea_20PerMiss_2VarMiss = rmsea_MCAR_20PerMiss_2VarMiss_2CR_SF, 
                            rmsea_50PerMiss_2VarMiss = rmsea_MCAR_50PerMiss_2VarMiss_2CR_SF, 
                            rmsea_20PerMiss_4VarMiss = rmsea_MCAR_20PerMiss_4VarMiss_2CR_SF,
                            rmsea_50PerMiss_4VarMiss = rmsea_MCAR_50PerMiss_4VarMiss_2CR_SF,
                            cfi_20PerMiss_2VarMiss = cfi_MCAR_20PerMiss_2VarMiss_2CR_SF, 
                            cfi_50PerMiss_2VarMiss = cfi_MCAR_50PerMiss_2VarMiss_2CR_SF, 
                            cfi_20PerMiss_4VarMiss = cfi_MCAR_20PerMiss_4VarMiss_2CR_SF,
                            cfi_50PerMiss_4VarMiss = cfi_MCAR_50PerMiss_4VarMiss_2CR_SF)
fitMCAR_Short_2CR_SF


save(fitMCAR_2CR_SF, file="fitMCAR_2CR_SF.RData")
save(fitMCAR_Short_2CR_SF, file="fitMCAR_Short_2CR_SF.RData" )

