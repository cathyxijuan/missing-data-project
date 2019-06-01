library(lavaan)
source("functions.R")
source("Models_WM.R")

### FOR TWO MISSING VARIABLES
#purpose: create missing data on x11,  x12.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMinPattern_2Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
  simuData[ind,7:8] <- NA
  simuData
}



#Usage: only for this research.  Two variables has missing data; maximum number of missing patterns for two variables: x11 and x12
# Each variable with missing data has the given percentage of missing data. 

#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern_2Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  for(i in 7:8){
    ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
    simuData[ind,i] <- NA
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
fit.ind.matrix.MCAR_2Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else  {
      simuData <- MCARMaxPattern_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } 
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", "baseline.df",
                                                                       "rmsea.ci.lower", "rmsea.ci.upper")])
  }
  
  fit.indices.MCAR
  
}




##FOR FOUR MISSING VARIABLES

#purpose: create missing data on x9 to x12.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMinPattern_4Var <- function(model, sample.nobs=1000000,  missing.percentage){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
  simuData[ind,7:10] <- NA
  simuData
}









#Usage: only for this research.  Four variables has missing data; max number of missing patterns for four variables: x9, x10, x11 and x12
# Each variable with missing data has the given percentage of missing data. 

#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern_4Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  for(i in 7:10){
    ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
    simuData[ind,i] <- NA
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
fit.ind.matrix.MCAR_4Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern_4Var(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else {
      simuData <- MCARMaxPattern_4Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } 
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", "baseline.df",
                                                                       "rmsea.ci.lower", "rmsea.ci.upper")])
  }
  
  fit.indices.MCAR
  
}


##FOR SIX MISSING VARIABLES

#purpose: create missing data on x7 to x12.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMinPattern_6Var <- function(model, sample.nobs=1000000,  missing.percentage){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
  simuData[ind,7:12] <- NA
  simuData
}






#Usage: only for this research.  Six variables has missing data; max number of missing patterns for six variables: x9, x10, x11 and x12
# Each variable with missing data has the given percentage of missing data. 

#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMaxPattern_6Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
  missing.percentage <- missing.percentage
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  for(i in 7:12){
    ind <- as.logical(rbinom(sample.nobs, 1, missing.percentage))
    simuData[ind,i] <- NA
  }
  simuData
}


#simuDatawithMiss <- MCARMaxPattern_6Var(pop.mod9)
#save(simuDatawithMiss, file="simuDatawithMiss.RData")
#summary(simuDatawithMiss)
#simuDataComplete <- simulateData(pop.mod9, sample.nobs=1000000, seed=111)
#save(simuDataComplete, file="simuDataComplete.RData")
# load("simuDatawithMiss.RData")
# fit <- cfa(fitted.mod, data=simuDatawithMiss, missing="fiml", mimic="EQS")
# lavInspect(fit, "fit")[c("fmin","rmsea","cfi")] #RMSEA=0.112; CFI=0.7640390
# 
# fit <- cfa(fitted.mod, data=simuDataComplete, missing="fiml", mimic="EQS")
# lavInspect(fit, "fit")[c("fmin","rmsea","cfi")] #RMSEA=0.1915; CFI=0.53785

#Usage: put fit indices for a list of models into a matrix

#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
#missing.type: a character: "min", "max", "max2". "max1"=each variable has the same proportion of missingness but the total proportion is different. 
############ "max2" = the total proportion of missingness is the same but the each variable has different proportions of missingness.
fit.ind.matrix.MCAR_6Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern_6Var(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else {
      simuData <- MCARMaxPattern_6Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } 
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", "baseline.df",
                                                                       "rmsea.ci.lower", "rmsea.ci.upper")])
  }
  
  fit.indices.MCAR
  
}




















fitMCAR_MinPat_20PerMiss_2VarMiss_WM <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")

fitMCAR_MinPat_50PerMiss_2VarMiss_WM <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")

fitMCAR_MaxPat_20PerMiss_2VarMiss_WM <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_2VarMiss_WM <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")

fitMCAR_MinPat_20PerMiss_4VarMiss_WM <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")

fitMCAR_MinPat_50PerMiss_4VarMiss_WM <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")

fitMCAR_MaxPat_20PerMiss_4VarMiss_WM <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_4VarMiss_WM <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")

fitMCAR_MinPat_20PerMiss_6VarMiss_WM <- 
  fit.ind.matrix.MCAR_6Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")

fitMCAR_MinPat_50PerMiss_6VarMiss_WM <- 
  fit.ind.matrix.MCAR_6Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")

fitMCAR_MaxPat_20PerMiss_6VarMiss_WM <- 
  fit.ind.matrix.MCAR_6Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_6VarMiss_WM <- 
  fit.ind.matrix.MCAR_6Var(pop.model.list=pop.mod, 
                           fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")




fitMCAR_WM <- list(MinPat_20PerMiss_2VarMiss=fitMCAR_MinPat_20PerMiss_2VarMiss_WM, 
                      MinPat_50PerMiss_2VarMiss=fitMCAR_MinPat_50PerMiss_2VarMiss_WM, 
                      MaxPat_20PerMiss_2VarMiss=fitMCAR_MaxPat_20PerMiss_2VarMiss_WM, 
                      MaxPat_50PerMiss_2VarMiss=fitMCAR_MaxPat_50PerMiss_2VarMiss_WM, 
                      MinPat_20PerMiss_4VarMiss=fitMCAR_MinPat_20PerMiss_4VarMiss_WM, 
                      MinPat_50PerMiss_4VarMiss=fitMCAR_MinPat_50PerMiss_4VarMiss_WM, 
                      MaxPat_20PerMiss_4VarMiss=fitMCAR_MaxPat_20PerMiss_4VarMiss_WM, 
                      MaxPat_50PerMiss_4VarMiss=fitMCAR_MaxPat_50PerMiss_4VarMiss_WM,
                     MinPat_20PerMiss_6VarMiss=fitMCAR_MinPat_20PerMiss_6VarMiss_WM, 
                     MinPat_50PerMiss_6VarMiss=fitMCAR_MinPat_50PerMiss_6VarMiss_WM, 
                     MaxPat_20PerMiss_6VarMiss=fitMCAR_MaxPat_20PerMiss_6VarMiss_WM, 
                     MaxPat_50PerMiss_6VarMiss=fitMCAR_MaxPat_50PerMiss_6VarMiss_WM) 

fitMCAR_WM 



rmsea_MCAR_MinPat_20PerMiss_2VarMiss_WM = rmsea_table(fitMCAR_MinPat_20PerMiss_2VarMiss_WM)
rmsea_MCAR_MinPat_50PerMiss_2VarMiss_WM = rmsea_table(fitMCAR_MinPat_50PerMiss_2VarMiss_WM)
rmsea_MCAR_MaxPat_20PerMiss_2VarMiss_WM = rmsea_table(fitMCAR_MaxPat_20PerMiss_2VarMiss_WM) 
rmsea_MCAR_MaxPat_50PerMiss_2VarMiss_WM = rmsea_table(fitMCAR_MaxPat_50PerMiss_2VarMiss_WM) 
rmsea_MCAR_MinPat_20PerMiss_4VarMiss_WM = rmsea_table(fitMCAR_MinPat_20PerMiss_4VarMiss_WM) 
rmsea_MCAR_MinPat_50PerMiss_4VarMiss_WM = rmsea_table(fitMCAR_MinPat_50PerMiss_4VarMiss_WM) 
rmsea_MCAR_MaxPat_20PerMiss_4VarMiss_WM = rmsea_table(fitMCAR_MaxPat_20PerMiss_4VarMiss_WM) 
rmsea_MCAR_MaxPat_50PerMiss_4VarMiss_WM = rmsea_table(fitMCAR_MaxPat_50PerMiss_4VarMiss_WM)
rmsea_MCAR_MinPat_20PerMiss_6VarMiss_WM = rmsea_table(fitMCAR_MinPat_20PerMiss_6VarMiss_WM) 
rmsea_MCAR_MinPat_50PerMiss_6VarMiss_WM = rmsea_table(fitMCAR_MinPat_50PerMiss_6VarMiss_WM) 
rmsea_MCAR_MaxPat_20PerMiss_6VarMiss_WM = rmsea_table(fitMCAR_MaxPat_20PerMiss_6VarMiss_WM) 
rmsea_MCAR_MaxPat_50PerMiss_6VarMiss_WM = rmsea_table(fitMCAR_MaxPat_50PerMiss_6VarMiss_WM)
cfi_MCAR_MinPat_20PerMiss_2VarMiss_WM = cfi_table(fitMCAR_MinPat_20PerMiss_2VarMiss_WM)
cfi_MCAR_MinPat_50PerMiss_2VarMiss_WM = cfi_table(fitMCAR_MinPat_50PerMiss_2VarMiss_WM)
cfi_MCAR_MaxPat_20PerMiss_2VarMiss_WM = cfi_table(fitMCAR_MaxPat_20PerMiss_2VarMiss_WM) 
cfi_MCAR_MaxPat_50PerMiss_2VarMiss_WM = cfi_table(fitMCAR_MaxPat_50PerMiss_2VarMiss_WM) 
cfi_MCAR_MinPat_20PerMiss_4VarMiss_WM = cfi_table(fitMCAR_MinPat_20PerMiss_4VarMiss_WM) 
cfi_MCAR_MinPat_50PerMiss_4VarMiss_WM = cfi_table(fitMCAR_MinPat_50PerMiss_4VarMiss_WM) 
cfi_MCAR_MaxPat_20PerMiss_4VarMiss_WM = cfi_table(fitMCAR_MaxPat_20PerMiss_4VarMiss_WM) 
cfi_MCAR_MaxPat_50PerMiss_4VarMiss_WM = cfi_table(fitMCAR_MaxPat_50PerMiss_4VarMiss_WM)
cfi_MCAR_MinPat_20PerMiss_6VarMiss_WM = cfi_table(fitMCAR_MinPat_20PerMiss_6VarMiss_WM) 
cfi_MCAR_MinPat_50PerMiss_6VarMiss_WM = cfi_table(fitMCAR_MinPat_50PerMiss_6VarMiss_WM) 
cfi_MCAR_MaxPat_20PerMiss_6VarMiss_WM = cfi_table(fitMCAR_MaxPat_20PerMiss_6VarMiss_WM) 
cfi_MCAR_MaxPat_50PerMiss_6VarMiss_WM = cfi_table(fitMCAR_MaxPat_50PerMiss_6VarMiss_WM)


fitMCAR_Short_WM <- list(rmsea_MinPat_20PerMiss_2VarMiss = rmsea_MCAR_MinPat_20PerMiss_2VarMiss_WM, 
                            rmsea_MinPat_50PerMiss_2VarMiss = rmsea_MCAR_MinPat_50PerMiss_2VarMiss_WM, 
                            rmsea_MaxPat_20PerMiss_2VarMiss = rmsea_MCAR_MaxPat_20PerMiss_2VarMiss_WM, 
                            rmsea_MaxPat_50PerMiss_2VarMiss = rmsea_MCAR_MaxPat_50PerMiss_2VarMiss_WM,
                            rmsea_MinPat_20PerMiss_4VarMiss = rmsea_MCAR_MinPat_20PerMiss_4VarMiss_WM,
                            rmsea_MinPat_50PerMiss_4VarMiss = rmsea_MCAR_MinPat_50PerMiss_4VarMiss_WM,
                            rmsea_MaxPat_20PerMiss_4VarMiss = rmsea_MCAR_MaxPat_20PerMiss_4VarMiss_WM,
                            rmsea_MaxPat_50PerMiss_4VarMiss = rmsea_MCAR_MaxPat_50PerMiss_4VarMiss_WM,
                          rmsea_MinPat_20PerMiss_6VarMiss = rmsea_MCAR_MinPat_20PerMiss_6VarMiss_WM,
                          rmsea_MinPat_50PerMiss_6VarMiss = rmsea_MCAR_MinPat_50PerMiss_6VarMiss_WM,
                          rmsea_MaxPat_20PerMiss_6VarMiss = rmsea_MCAR_MaxPat_20PerMiss_6VarMiss_WM,
                          rmsea_MaxPat_50PerMiss_6VarMiss = rmsea_MCAR_MaxPat_50PerMiss_6VarMiss_WM,
                            cfi_MinPat_20PerMiss_2VarMiss = cfi_MCAR_MinPat_20PerMiss_2VarMiss_WM, 
                            cfi_MinPat_50PerMiss_2VarMiss = cfi_MCAR_MinPat_50PerMiss_2VarMiss_WM, 
                            cfi_MaxPat_20PerMiss_2VarMiss = cfi_MCAR_MaxPat_20PerMiss_2VarMiss_WM, 
                            cfi_MaxPat_50PerMiss_2VarMiss = cfi_MCAR_MaxPat_50PerMiss_2VarMiss_WM,
                            cfi_MinPat_20PerMiss_4VarMiss = cfi_MCAR_MinPat_20PerMiss_4VarMiss_WM,
                            cfi_MinPat_50PerMiss_4VarMiss = cfi_MCAR_MinPat_50PerMiss_4VarMiss_WM,
                            cfi_MaxPat_20PerMiss_4VarMiss = cfi_MCAR_MaxPat_20PerMiss_4VarMiss_WM,
                            cfi_MaxPat_50PerMiss_4VarMiss = cfi_MCAR_MaxPat_50PerMiss_4VarMiss_WM,
                          cfi_MinPat_20PerMiss_6VarMiss = cfi_MCAR_MinPat_20PerMiss_6VarMiss_WM,
                          cfi_MinPat_50PerMiss_6VarMiss = cfi_MCAR_MinPat_50PerMiss_6VarMiss_WM,
                          cfi_MaxPat_20PerMiss_6VarMiss = cfi_MCAR_MaxPat_20PerMiss_6VarMiss_WM,
                          cfi_MaxPat_50PerMiss_6VarMiss = cfi_MCAR_MaxPat_50PerMiss_6VarMiss_WM)
fitMCAR_Short_WM


save(fitMCAR_WM, file="fitMCAR_WM.RData" )
save(fitMCAR_Short_WM, file="fitMCAR_Short_WM.RData" )
