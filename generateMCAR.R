library(lavaan)
source("functions.R")
source("Models_CR1_1.R")

### FOR TWO MISSING VARIABLES
#purpose: create missing data on x11,  x12.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
MCARMinPattern_2Var <- function(model, sample.nobs=1000000,  missing.percentage){
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
MCARMaxPattern_2Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
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
MCARMaxPattern2_2Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
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
fit.ind.matrix.MCAR_2Var <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  fit.indices.MCAR <-matrix( nrow = 0, ncol = 12)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type =="min"){
      simuData <- MCARMinPattern_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)} 
    else if (missing.type == "max"){
      simuData <- MCARMaxPattern_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {
      simuData <- MCARMaxPattern2_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    }
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MCAR<- rbind(fit.indices.MCAR,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", 
                                                                       "chisq", "pvalue", "baseline.chisq", "baseline.df",
                                                                       "rmsea.ci.lower", "rmsea.ci.upper")])
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
MCARMinPattern_4Var <- function(model, sample.nobs=1000000,  missing.percentage){
  data <- simulateData(model, sample.nobs=sample.nobs,seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  simuData[1:(sample.nobs*missing.percentage),  c(9:12)] <-NA
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
MCARMaxPattern_4Var <- function(model, sample.nobs=1000000,  missing.percentage=.5){
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







fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")

fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")

fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")

fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "min")

fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "min")

fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "max")

fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1 <- 
  fit.ind.matrix.MCAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "max")

fitMCAR_CR1_1 <- list(MinPat_20PerMiss_2VarMiss=fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1, 
                    MinPat_50PerMiss_2VarMiss=fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1, 
                    MaxPat_20PerMiss_2VarMiss=fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1, 
                    MaxPat_50PerMiss_2VarMiss=fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1, 
                    MinPat_20PerMiss_4VarMiss=fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_1, 
                    MinPat_50PerMiss_4VarMiss=fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1, 
                    MaxPat_20PerMiss_4VarMiss=fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_1, 
                    MaxPat_50PerMiss_4VarMiss=fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1) 

fitMCAR_CR1_1 



rmsea_MCAR_MinPat_20PerMiss_2VarMiss_CR1_1 = rmsea_table(fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1)
rmsea_MCAR_MinPat_50PerMiss_2VarMiss_CR1_1 = rmsea_table(fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1)
rmsea_MCAR_MaxPat_20PerMiss_2VarMiss_CR1_1 = rmsea_table(fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1) 
rmsea_MCAR_MaxPat_50PerMiss_2VarMiss_CR1_1 = rmsea_table(fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1) 
rmsea_MCAR_MinPat_20PerMiss_4VarMiss_CR1_1 = rmsea_table(fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_1) 
rmsea_MCAR_MinPat_50PerMiss_4VarMiss_CR1_1 = rmsea_table(fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1) 
rmsea_MCAR_MaxPat_20PerMiss_4VarMiss_CR1_1 = rmsea_table(fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_1) 
rmsea_MCAR_MaxPat_50PerMiss_4VarMiss_CR1_1 = rmsea_table(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1)
cfi_MCAR_MinPat_20PerMiss_2VarMiss_CR1_1 = cfi_table(fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1)
cfi_MCAR_MinPat_50PerMiss_2VarMiss_CR1_1 = cfi_table(fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1)
cfi_MCAR_MaxPat_20PerMiss_2VarMiss_CR1_1 = cfi_table(fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1) 
cfi_MCAR_MaxPat_50PerMiss_2VarMiss_CR1_1 = cfi_table(fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1) 
cfi_MCAR_MinPat_20PerMiss_4VarMiss_CR1_1 = cfi_table(fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_1) 
cfi_MCAR_MinPat_50PerMiss_4VarMiss_CR1_1 = cfi_table(fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1) 
cfi_MCAR_MaxPat_20PerMiss_4VarMiss_CR1_1 = cfi_table(fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_1) 
cfi_MCAR_MaxPat_50PerMiss_4VarMiss_CR1_1 = cfi_table(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1)


fitMCAR_Short_CR1_1 <- list(rmsea_MinPat_20PerMiss_2VarMiss = rmsea_MCAR_MinPat_20PerMiss_2VarMiss_CR1_1, 
                          rmsea_MinPat_50PerMiss_2VarMiss = rmsea_MCAR_MinPat_50PerMiss_2VarMiss_CR1_1, 
                          rmsea_MaxPat_20PerMiss_2VarMiss = rmsea_MCAR_MaxPat_20PerMiss_2VarMiss_CR1_1, 
                          rmsea_MaxPat_50PerMiss_2VarMiss = rmsea_MCAR_MaxPat_50PerMiss_2VarMiss_CR1_1,
                          rmsea_MinPat_20PerMiss_4VarMiss = rmsea_MCAR_MinPat_20PerMiss_4VarMiss_CR1_1,
                          rmsea_MinPat_50PerMiss_4VarMiss = rmsea_MCAR_MinPat_50PerMiss_4VarMiss_CR1_1,
                          rmsea_MaxPat_20PerMiss_4VarMiss = rmsea_MCAR_MaxPat_20PerMiss_4VarMiss_CR1_1,
                          rmsea_MaxPat_50PerMiss_4VarMiss = rmsea_MCAR_MaxPat_50PerMiss_4VarMiss_CR1_1,
                          cfi_MinPat_20PerMiss_2VarMiss = cfi_MCAR_MinPat_20PerMiss_2VarMiss_CR1_1, 
                          cfi_MinPat_50PerMiss_2VarMiss = cfi_MCAR_MinPat_50PerMiss_2VarMiss_CR1_1, 
                          cfi_MaxPat_20PerMiss_2VarMiss = cfi_MCAR_MaxPat_20PerMiss_2VarMiss_CR1_1, 
                          cfi_MaxPat_50PerMiss_2VarMiss = cfi_MCAR_MaxPat_50PerMiss_2VarMiss_CR1_1,
                          cfi_MinPat_20PerMiss_4VarMiss = cfi_MCAR_MinPat_20PerMiss_4VarMiss_CR1_1,
                          cfi_MinPat_50PerMiss_4VarMiss = cfi_MCAR_MinPat_50PerMiss_4VarMiss_CR1_1,
                          cfi_MaxPat_20PerMiss_4VarMiss = cfi_MCAR_MaxPat_20PerMiss_4VarMiss_CR1_1,
                          cfi_MaxPat_50PerMiss_4VarMiss = cfi_MCAR_MaxPat_50PerMiss_4VarMiss_CR1_1)
fitMCAR_Short_CR1_1


save(fitMCAR_CR1_1, file="fitMCAR_CR1_1.RData" )
save(fitMCAR_Short_CR1_1, file="fitMCAR_Short_CR1_1.RData" )

