library(lavaan)
library(tmvtnorm)
library(simsem)


#The population model which is consistent with the population covariance matrix. 
pop.mod <- '    
y~~ 1*y
x~~2*x
x~~0.5*y
'
#our hypothesized model 
ana.mod <- '     
y~~0*x
y~~1*y
x~~2*x'


n=1000000 #Generating two large datasets
simudata_orig<- simulateData(pop.mod , sample.nobs=1000000,seed=111)


##checking the F value of the complete data
fit_complete <-sem(ana.mod, data=simudata_orig, missing="fiml", mimic="EQS")
F_complete <- lavInspect(fit_complete, "fit")["fmin"]*2 
F_complete 

#creating a dataset with MAR missing values
simudata <- simudata_orig #subsetting the second dataset for creating a dataset with missing values
sigma.comp <- cov(simudata)
simudata[simudata[,2] > 0,1] <- NA #creating the dataset with missing values. The rule I used is "when the X2 is greater than 0, X1 is missing"
head(simudata, 15) #checking the dataset with missing values. 



n.missing <- nrow(simudata[is.na(simudata[,1]),])
n.missing #approximately 500687 missing values; very close to 50% of the dataset. 
percentage.missing <- n.missing /n
percentage.missing


#fitting the data with missing value
fit <-cfa(ana.mod, data=simudata, mimic="EQS", missing="fiml")

F_true <- lavInspect(fit, "fit")["fmin"]*2 
F_true




##Now I will subsetting different matrices for calculating F value. 
#There are two missing data patterns:1) When X2 <0, X1 is not missing. 2) When X2 >0, X1 is missing

##Calculating the matries for the first pattern
##subsetting the first data pattern:
simu_comp <- simudata[is.na(simudata[,1]) ==F,]
#calculating the truncated covarince matrix and means for this pattern.
sigma_truncated1 <- cov(simu_comp)*(nrow(simu_comp)-1)/nrow(simu_comp)
mu_truncated1 <- sapply(simu_comp, mean)
#calculating the EM covarince matrix and means for this pattern.
sigma_EM1 <- lavInspect(fit, "sampstat.h1")$cov
mu_EM1 <- lavInspect(fit, "sampstat.h1")$mean
#the model implied covariance matrix and means
sigma_hat1 <- lavInspect(fit, "cov.ov")
mu_hat1 <- lavInspect(fit, "mean.ov")



##Calculating the matries for the second pattern
##subsetting the second data pattern:
simu_missing <- simudata[is.na(simudata[,1]),2]
head(simu_missing, 15)
#calculating the truncated covarince matrix and means for this pattern.
sigma_truncated2 <- var(simu_missing)*(length(simu_missing)-1)/length(simu_missing)
mu_truncated2 <- mean(simu_missing)
#calculating the EM covarince matrix and means for this pattern.
sigma_EM2 <-  lavInspect(fit, "sampstat.h1")$cov[-1,-1]
mu_EM2 <-lavInspect(fit, "sampstat.h1")$mean[2]
#the model implied covariance matrix and means
sigma_hat2 <- sigma_hat1[2, 2]
mu_hat2 <- lavInspect(fit, "mean.ov")[2]



#F value for the first pattern 
F_1 <- log(det(sigma_hat1%*%solve(sigma_EM1)))  +
  sum(diag((sigma_truncated1 +(mu_truncated1-mu_hat1)%*%t(mu_truncated1-mu_hat1))%*%solve(sigma_hat1))) -
  sum(diag((sigma_truncated1 + (mu_truncated1-mu_EM1)%*%t(mu_truncated1-mu_EM1))%*%solve(sigma_EM1)))
#F value for the second pattern 
F_2<- log(det(sigma_hat2%*%solve(sigma_EM2))) +
  sum(diag((sigma_truncated2 + (mu_truncated2-mu_hat2)%*%t(mu_truncated2-mu_hat2))%*%solve(sigma_hat2)))-
  sum(diag((sigma_truncated2 + (mu_truncated2-mu_EM2)%*%t(mu_truncated2-mu_EM2))%*%solve(sigma_EM2)))
#weighing those two patterns to get the final F
F_total <- (1-percentage.missing)*F_1+(percentage.missing)*F_2
F_total
F_true 




