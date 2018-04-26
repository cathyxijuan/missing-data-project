library(lavaan)
source("Models_CR2_3.R")

##four missing variables

#strong dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 variables. 
#       Creating missing data on x11 and x12. Strong dependence:missing of x9 and x11 depends on x7; missing of x10 and x12 depends on x8.
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARStrong <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
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

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 variables. 
#       Creating missing data on x9, x10, x11 and x12. 
#       Weak dependence:missing of x9 and x11 depends on x7; missing of x10 and x12 depends on x8. 75% of data beyond a cutoff are eliminated.
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARWeak <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
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
fit.ind.matrix.MAR <- function(pop.model.list, fitted.mod, sample.nobs=1000000,  missing.percentage, missing.type){
  fit.indices.MAR <-matrix( nrow = 0, ncol = 6)
  
  for(i in 1:length(pop.model.list)){
    if(missing.type=="strong"){
      simuData <- MARStrong(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {  simuData <- MARWeak(pop.model.list[[i]], sample.nobs, missing.percentage)}
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    fit.indices.MAR<- rbind(fit.indices.MAR, lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df")])
  }
  
  fit.indices.MAR
  
}


#Usage: put sigma.hat for a list of models into a list

#Arguments: same as above

sigma.hat.MAR <- function(pop.model.list, fitted.mod, sample.nobs = 1000000,  missing.percentage, missing.type){
  sigma.hat <-list()
  
  for(i in 1:length(pop.model.list)){
    if(missing.type=="strong"){
      simuData <- MARStrong(pop.model.list[[i]], sample.nobs, missing.percentage)
    } else {  simuData <- MARWeak(pop.model.list[[i]], sample.nobs, missing.percentage)}
    fit <- cfa(fitted.mod, data=simuData, missing="fiml", mimic="EQS")
    sigma.hat[[i]]<- lavInspect(fit, "cov.ov")
  }
  
  sigma.hat
  
}







sigmaHat_MAR_Strong_20PerMiss_4VarMiss_CR2_3 <-
  sigma.hat.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "strong")

sigmaHat_MAR_Strong_50PerMiss_4VarMiss_CR2_3 <-
  sigma.hat.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "strong")
# 
# fitMAR_Strong_20PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "strong")
# 
# fitMAR_Strong_50PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "strong")
# 
# 
# round(fitMAR_Strong_20PerMiss_4VarMiss_CR2_3,6) 
# 
# round(fitMAR_Strong_50PerMiss_4VarMiss_CR2_3,6)
# 
# 
# 
save(sigmaHat_MAR_Strong_20PerMiss_4VarMiss_CR2_3, file="sigmaHat_MAR_Strong_20PerMiss_4VarMiss_CR2_3.RData")
save(sigmaHat_MAR_Strong_50PerMiss_4VarMiss_CR2_3, file="sigmaHat_MAR_Strong_50PerMiss_4VarMiss_CR2_3.RData")
# save(fitMAR_Strong_20PerMiss_4VarMiss_CR2_3, file="fitMAR_Strong_20PerMiss_4VarMiss_CR2_3.RData")
# save(fitMAR_Strong_50PerMiss_4VarMiss_CR2_3, file="fitMAR_Strong_50PerMiss_4VarMiss_CR2_3.RData")





sigmaHat_MAR_Weak_20PerMiss_4VarMiss_CR2_3 <- 
  sigma.hat.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "weak")

sigmaHat_MAR_Weak_50PerMiss_4VarMiss_CR2_3 <- 
  sigma.hat.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "weak")

# fitMAR_Weak_20PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.20, missing.type = "weak")
# 
# fitMAR_Weak_50PerMiss_4VarMiss_CR2_3 <- 
#   fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage = 0.50, missing.type = "weak")



save(sigmaHat_MAR_Weak_20PerMiss_4VarMiss_CR2_3, file="sigmaHat_MAR_Weak_20PerMiss_4VarMiss_CR2_3.RData")
save(sigmaHat_MAR_Weak_50PerMiss_4VarMiss_CR2_3, file="sigmaHat_MAR_Weak_50PerMiss_4VarMiss_CR2_3.RData")
# save(fitMAR_Weak_20PerMiss_4VarMiss_CR2_3, file="fitMAR_Weak_20PerMiss_4VarMiss_CR2_3.RData")
# save(fitMAR_Weak_50PerMiss_4VarMiss_CR2_3, file="fitMAR_Weak_50PerMiss_4VarMiss_CR2_3.RData")

