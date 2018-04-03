library(lavaan)
source("Models_CR.R")

##Two missing variables

#strong dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 variables. Creating missing data on x11 and x12.
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
  
  for(i in 0:2){
    simuData[simuData[,4+i] > cutoff,1+i] <- NA
    simuData[simuData[,10+i] > cutoff,7+i] <- NA
  }
  simuData
}


#weak dependency

#Usage: FOR THIS RESEARCH ONLY. There need to be 12 variables. 
#Argument:
#model: lavaan defined population model
#sample.nobs: numeric; sample size without missing data
#missing.percentage: numeric; a proportion of missing data
MARWeak <- function(model, sample.nobs=1000000,  missing.percentage=0.5){
  data <- simulateData(model, sample.nobs=sample.nobs, seed=111)
  simuData <- data.frame(x1=data[,"x1"], x2=data[,"x2"], x3=data[,"x3"], x4=data[,"x4"],
                         x5=data[,"x5"], x6=data[,"x6"], x7=data[,"x7"], x8=data[,"x8"],
                         x9=data[,"x9"], x10=data[,"x10"], x11=data[,"x11"], x12=data[,"x12"])
  
  cutoff<- qnorm(missing.percentage, lower.tail = F)
  for(i in 0:2){
    ind <- which(simuData[,4+i] > cutoff)
    keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
    row.keep <- ind[keep.log]
    ind2 <- which(simuData[,4+i] < cutoff)
    keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
    row.keep2 <- ind2[keep.log2]
    simuData[c(row.keep, row.keep2),1+i] <-NA ##Note the rows that are kept got deleted. So it really should be called row.delete.
    
    ind <- which(simuData[,10+i] > cutoff)
    keep.log <- as.logical(sample(0:1,length(ind),replace=T, prob=c(0.25, 0.75))) #1=delete
    row.keep <- ind[keep.log]
    ind2 <- which(simuData[,10+i] < cutoff)
    keep.log2 <- as.logical(sample(0:1,length(ind2),replace=T, prob=c(0.75, 0.25))) #1=delete
    row.keep2 <- ind2[keep.log2]
    simuData[c(row.keep, row.keep2),7+i] <-NA
  }
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



fit.MAR.STRONG50.CR <- fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage=0.50, missing.type = "strong")
fit.MAR.STRONG20.CR <-fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage=0.20, missing.type = "strong")


fit.MAR.WEAK20.CR <- fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage=0.20, missing.type = "weak")

fit.MAR.WEAK50.CR <- fit.ind.matrix.MAR(pop.model.list=pop.mod, fitted.mod=fitted.mod, missing.percentage=0.50, missing.type = "weak")

fit.MAR.STRONG50.CR
fit.MAR.STRONG20.CR 
fit.MAR.WEAK50.CR
fit.MAR.WEAK20.CR


# 
# save(fit.MAR.STRONG50.CR,file= "fit.MAR.STRONG50.CR.RData")
# save(fit.MAR.STRONG20.CR , file="fit.MAR.STRONG20.CR.RData")
# save(fit.MAR.WEAK50.CR, file="fit.MAR.WEAK50.CR.RData")
# save(fit.MAR.WEAK20.CR, file="fit.MAR.WEAK20.CR.RData")
 # 
