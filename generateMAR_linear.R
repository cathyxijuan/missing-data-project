library(lavaan)
source("functions.R")
source("Models_CR2_3.R")


#strong dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 Variables. 
#       Creating missing data on x11 and x12. Strong dependence:missing of x11 depends on x7; missing of x12 depends on x8
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARStrong_2Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  cutoff<- qnorm(missing.percentage, lower.tail = F)
  
  simuData[simuData[,7] > cutoff,11] <- NA
  simuData[simuData[,8] > cutoff,12] <- NA
  
  simuData
}







#weak dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 Variables. 
#       Creating missing data on x11 and x12. Weak dependence:missing of x11 depends on x7; missing of x12 depends on x8. 75% of data beyond a cutoff are eliminated.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARWeak_2Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  cutoff<- qnorm(missing.percentage, lower.tail = F)
  #create missing for x11
  ind <- which(simuData[,7] > cutoff)
  keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
  row.keep <- ind[keep.log]
  ind2 <- which(simuData[,7] < cutoff)
  keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
  row.keep2 <- ind2[keep.log2]
  simuData[c(row.keep, row.keep2),11] <-NA ##Note the rows that are kept got deleted. So it really should be called row.delete.
  
  #create missing for x12
  ind <- which(simuData[,8] > cutoff)
  keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
  row.keep <- ind[keep.log]
  ind2 <- which(simuData[,8] < cutoff)
  keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
  row.keep2 <- ind2[keep.log2]
  simuData[c(row.keep, row.keep2),12] <-NA
  
  simuData
}



#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
#missing.type: a character: "strong" or "weak"
fit.ind.matrix.MAR_2Var <- function(pop.model.list, fitted.mod, sample.nobs=1000000,  missing.percentage, missing.type){
  fit.indices.MAR <-matrix( nrow = 0, ncol = 12)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type=="strong"){
      simuData <- MARStrong_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {  simuData <- MARWeak_2Var(pop.model.list[[i]], sample.nobs, missing.percentage)}
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MAR<- rbind(fit.indices.MAR, lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", "chisq", "pvalue", "baseline.chisq", "baseline.df", "rmsea.ci.lower", "rmsea.ci.upper")])
  }
  fit.indices.MAR
}


#strong dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 Variables. 
#       Creating missing data on x11 and x12. Strong dependence:missing of x9 and x11 depends on x7; missing of x10 and x12 depends on x8.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARStrong_4Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  cutoff<- qnorm(missing.percentage, lower.tail = F)
  
  simuData[simuData[,7] > cutoff,c(9,11)] <- NA
  simuData[simuData[,8] > cutoff,c(10,12)] <- NA
  
  simuData
}







#weak dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 Variables. 
#       Creating missing data on x9, x10, x11 and x12. 
#       Weak dependence:missing of x9 and x11 depends on x7; missing of x10 and x12 depends on x8. 75% of data beyond a cutoff are eliminated.
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARWeak_4Var <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  cutoff<- qnorm(missing.percentage, lower.tail = F)
  #create missing for x11
  ind <- which(simuData[,7] > cutoff)
  keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
  row.keep <- ind[keep.log]
  ind2 <- which(simuData[,7] < cutoff)
  keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
  row.keep2 <- ind2[keep.log2]
  simuData[c(row.keep, row.keep2),c(9,11)] <-NA ##Note the rows that are kept got deleted. So it really should be called row.delete.
  
  #create missing for x12
  ind <- which(simuData[,8] > cutoff)
  keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
  row.keep <- ind[keep.log]
  ind2 <- which(simuData[,8] < cutoff)
  keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
  row.keep2 <- ind2[keep.log2]
  simuData[c(row.keep, row.keep2),c(10,12)] <-NA
  
  simuData
}





#Arguments:
#pop.model.list: a list of lavaan models for the population
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
#missing.percentage: vector specifying which columns are missing
#missing.type: a character: "strong" or "weak"
fit.ind.matrix.MAR_4Var <- function(pop.model.list, fitted.mod, sample.nobs=1000000,  missing.percentage, missing.type){
  fit.indices.MAR <-matrix( nrow = 0, ncol = 6)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type=="strong"){
      simuData <- MARStrong_4Var(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {  simuData <- MARWeak_4Var(pop.model.list[[i]], sample.nobs, missing.percentage)}
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MAR<- rbind(fit.indices.MAR, lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df")])
  }
  fit.indices.MAR
}





fitMAR_Strong_20PerMiss_2VarMiss_CR2_3 <-
  fit.ind.matrix.MAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "strong")

fitMAR_Strong_50PerMiss_2VarMiss_CR2_3 <-
  fit.ind.matrix.MAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "strong")

fitMAR_Weak_20PerMiss_2VarMiss_CR2_3 <- 
  fit.ind.matrix.MAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "weak")

fitMAR_Weak_50PerMiss_2VarMiss_CR2_3 <- 
  fit.ind.matrix.MAR_2Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "weak")

fitMAR_Strong_20PerMiss_4VarMiss_CR2_3 <-
  fit.ind.matrix.MAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "strong")

fitMAR_Strong_50PerMiss_4VarMiss_CR2_3 <-
  fit.ind.matrix.MAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "strong")

fitMAR_Weak_20PerMiss_4VarMiss_CR2_3 <- 
  fit.ind.matrix.MAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "weak")

fitMAR_Weak_50PerMiss_4VarMiss_CR2_3 <- 
  fit.ind.matrix.MAR_4Var(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "weak")






fitMAR_CR2_3 <- list(Strong_20PerMiss_2VarMiss = fitMAR_Strong_20PerMiss_2VarMiss_CR2_3,
                     Strong_50PerMiss_2VarMiss = fitMAR_Strong_50PerMiss_2VarMiss_CR2_3, 
                     Weak_20PerMiss_2VarMiss = fitMAR_Weak_20PerMiss_2VarMiss_CR2_3,
                     Weak_50PerMiss_2VarMiss = fitMAR_Weak_50PerMiss_2VarMiss_CR2_3,
                     Strong_20PerMiss_4VarMiss = fitMAR_Strong_20PerMiss_4VarMiss_CR2_3,
                     Strong_50PerMiss_4VarMiss = fitMAR_Strong_50PerMiss_4VarMiss_CR2_3,
                     Weak_20PerMiss_4VarMiss = fitMAR_Weak_20PerMiss_4VarMiss_CR2_3,
                     Weak_50PerMiss_4VarMiss = fitMAR_Weak_50PerMiss_4VarMiss_CR2_3) 


round(fitMAR_CR2_3,6)


rmsea_MAR_Strong_20PerMiss_2VarMiss_CR2_3 = rmsea_table(fitMAR_Strong_20PerMiss_2VarMiss_CR2_3)
rmsea_MAR_Strong_50PerMiss_2VarMiss_CR2_3 = rmsea_table(fitMAR_Strong_50PerMiss_2VarMiss_CR2_3) 
rmsea_MAR_Weak_20PerMiss_2VarMiss_CR2_3 = rmsea_table(fitMAR_Weak_20PerMiss_2VarMiss_CR2_3)
rmsea_MAR_Weak_50PerMiss_2VarMiss_CR2_3 = rmsea_table(fitMAR_Weak_50PerMiss_2VarMiss_CR2_3)
rmsea_MAR_Strong_20PerMiss_4VarMiss_CR2_3 = rmsea_table(fitMAR_Strong_20PerMiss_4VarMiss_CR2_3)
rmsea_MAR_Strong_50PerMiss_4VarMiss_CR2_3 = rmsea_table(fitMAR_Strong_50PerMiss_4VarMiss_CR2_3)
rmsea_MAR_Weak_20PerMiss_4VarMiss_CR2_3 = rmsea_table(fitMAR_Weak_20PerMiss_4VarMiss_CR2_3)
rmsea_MAR_Weak_50PerMiss_4VarMiss_CR2_3 = rmsea_table(fitMAR_Weak_50PerMiss_4VarMiss_CR2_3) 
cfi_MAR_Strong_20PerMiss_2VarMiss_CR2_3 = cfi_table(fitMAR_Strong_20PerMiss_2VarMiss_CR2_3)
cfi_MAR_Strong_50PerMiss_2VarMiss_CR2_3 = cfi_table(fitMAR_Strong_50PerMiss_2VarMiss_CR2_3) 
cfi_MAR_Weak_20PerMiss_2VarMiss_CR2_3 = cfi_table(fitMAR_Weak_20PerMiss_2VarMiss_CR2_3)
cfi_MAR_Weak_50PerMiss_2VarMiss_CR2_3 = cfi_table(fitMAR_Weak_50PerMiss_2VarMiss_CR2_3)
cfi_MAR_Strong_20PerMiss_4VarMiss_CR2_3 = cfi_table(fitMAR_Strong_20PerMiss_4VarMiss_CR2_3)
cfi_MAR_Strong_50PerMiss_4VarMiss_CR2_3 = cfi_table(fitMAR_Strong_50PerMiss_4VarMiss_CR2_3)
cfi_MAR_Weak_20PerMiss_4VarMiss_CR2_3 = cfi_table(fitMAR_Weak_20PerMiss_4VarMiss_CR2_3)
cfi_MAR_Weak_50PerMiss_4VarMiss_CR2_3 = cfi_table(fitMAR_Weak_50PerMiss_4VarMiss_CR2_3) 



fitMAR_Short_CR2_3 <-list(rmsea_Strong_20PerMiss_2VarMiss = rmsea_MAR_Strong_20PerMiss_2VarMiss_CR2_3, 
                          rmsea_Strong_50PerMiss_2VarMiss = rmsea_MAR_Strong_50PerMiss_2VarMiss_CR2_3,
                          rmsea_Weak_20PerMiss_2VarMiss = rmsea_MAR_Weak_20PerMiss_2VarMiss_CR2_3,
                          rmsea_Weak_50PerMiss_2VarMiss = rmsea_MAR_Weak_50PerMiss_2VarMiss_CR2_3,
                          rmsea_Strong_20PerMiss_4VarMiss = rmsea_MAR_Strong_20PerMiss_4VarMiss_CR2_3,
                          rmsea_Strong_50PerMiss_4VarMiss = rmsea_MAR_Strong_50PerMiss_4VarMiss_CR2_3,
                          rmsea_Weak_20PerMiss_4VarMiss = rmsea_MAR_Weak_20PerMiss_4VarMiss_CR2_3,
                          rmsea_Weak_50PerMiss_4VarMiss = rmsea_MAR_Weak_50PerMiss_4VarMiss_CR2_3,
                          cfi_Strong_20PerMiss_2VarMiss = cfi_MAR_Strong_20PerMiss_2VarMiss_CR2_3,
                          cfi_Strong_50PerMiss_2VarMiss = cfi_MAR_Strong_50PerMiss_2VarMiss_CR2_3,
                          cfi_Weak_20PerMiss_2VarMiss = cfi_MAR_Weak_20PerMiss_2VarMiss_CR2_3,
                          cfi_Weak_50PerMiss_2VarMiss = cfi_MAR_Weak_50PerMiss_2VarMiss_CR2_3,
                          cfi_Strong_20PerMiss_4VarMiss = cfi_MAR_Strong_20PerMiss_4VarMiss_CR2_3,
                          cfi_Strong_50PerMiss_4VarMiss = cfi_MAR_Strong_50PerMiss_4VarMiss_CR2_3,
                          cfi_Weak_20PerMiss_4VarMiss = cfi_MAR_Weak_20PerMiss_4VarMiss_CR2_3,
                          cfi_Weak_50PerMiss_4VarMiss = cfi_MAR_Weak_50PerMiss_4VarMiss_CR2_3)



save(fitMAR_CR2_3, file="fitMAR_CR2_3.RData")
save(fitMAR_Short_CR2_3, file="fitMAR_Short_CR2_3.RData")
