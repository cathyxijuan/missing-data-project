library(lavaan)
library(tmvtnorm)
library(simsem)




#The population model which is consistent with the population covariance matrix. 
pop.mod <- '    
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6
x2 ~~0.4*x3
'
#our hypothesized model 
ana.mod <- '     
f1 =~ .7*x1 + .7*x2 + .7*x3 
f2 =~ .7*x4 + .7*x5 + .7*x6 
f1 ~~ 0*f2
f1 ~~ 1*f1
f2 ~~ 1*f2    
x1 ~~ .51*x1
x2 ~~ .51*x2
x3 ~~ .51*x3
x4 ~~ .51*x4
x5 ~~ .51*x5
x6 ~~ .51*x6'

fit <-cfa(pop.mod, data=simulateData(pop.mod, sample.nobs=1000))
sigma<- lavInspect(fit, "cov.ov")

n=1000000 #Generating two large datasets
simudata_orig<- sim(nRep=3, ana.mod, n=n, generate=pop.mod, lavaanfun="sem", dataOnly = T)
head(simudata_orig[[2]], 15) #I will be using the second dataset for this demonstration. 
str(lavaanify(ana.mod))
lavParTable(ana.mod)

##checking the F value of the complete data
fit_complete <-cfa(ana.mod, data=simudata_orig[[2]], mimic="EQS", missing="fiml")
F_complete <- lavInspect(fit_complete, "fit")["fmin"]*2 
F_complete 

#creating a dataset with MAR missing values
simudata <- simudata_orig[[2]] #subsetting the second dataset for creating a dataset with missing values
sigma.comp <- cov(simudata)
simudata[simudata[,2] > 0,1] <- NA #creating the dataset with missing values. The rule I used is "when the X2 is greater than 0, X1 is missing"
head(simudata, 15) #checking the dataset with missing values. 


#Some sanity checks before calculating F value. 
#  1) compare the sample covariance matrix with the population covariance matrix with the complete data
cov(simudata_orig[[2]])
sigma
#  2) compare variable means in complete data or missing data
sapply(simudata_orig[[2]], mean)
sapply(simudata, mean, na.rm=T) #because of the truncated data, X1 does not that a mean of 0
# 3) looking at the number of rows with missing data
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
simu_missing <- simudata[is.na(simudata[,1]),2:6]
head(simu_missing, 15)
#calculating the truncated covarince matrix and means for this pattern.
sigma_truncated2 <- cov(simu_missing)*(nrow(simu_missing)-1)/nrow(simu_missing)
mu_truncated2 <- sapply(simu_missing, mean)
#calculating the EM covarince matrix and means for this pattern.
sigma_EM2 <-  lavInspect(fit, "sampstat.h1")$cov[-1,-1]
mu_EM2 <-lavInspect(fit, "sampstat.h1")$mean[2:6]
#the model implied covariance matrix and means
sigma_hat2 <- sigma_hat1[2:12, 2:12]
mu_hat2 <- lavInspect(fit, "mean.ov")[2:12]




n1 <-as.matrix(simu_comp[1,]-mu_hat1)
n1%*%as.matrix(sigma_hat1)%*%t(n1)

sum_hat <- function(x) {
  n <- as.matrix(x-mu_hat1)
  n%*%as.matrix(sigma_hat1)%*%t(n)
}

samp_hat1 <- apply(simu_comp, 1, function(x) t(x-mu_hat1)%*%solve(sigma_hat1)%*%(x-mu_hat1) )
sample_hat1 <- sum(samp_hat1)/length(samp_hat1)
samp_hat2 <- apply(simu_missing, 1, function(x) t(x-mu_hat2)%*%solve(sigma_hat2)%*%(x-mu_hat2) )
sample_hat2 <- sum(samp_hat2)/length(samp_hat2)
samp_EM1 <- apply(simu_comp, 1, function(x) t(x-mu_EM1)%*%solve(sigma_EM1)%*%(x-mu_EM1) )
sample_EM1 <- sum(samp_EM1)/length(samp_EM1)
samp_EM2 <- apply(simu_missing, 1, function(x) t(x-mu_EM2)%*%solve(sigma_EM2)%*%(x-mu_EM2) )
sample_EM2 <- sum(samp_EM2)/length(samp_EM2)

#!
nf <- (n-n.missing)*log(det(sigma_hat1%*%solve(sigma_EM1))) + n.missing*log(det(sigma_hat2%*%solve(sigma_EM2))) + sum(samp_hat1) + sum(samp_hat2) - sum(samp_EM1)-sum(samp_EM2)
f <- nf/n
f

#2
p1 <- (1-percentage.missing)*(log(det(sigma_hat1%*%solve(sigma_EM1)))+
                                sample_hat1 -
                                sample_EM1)
p2 <- percentage.missing*(log(det(sigma_hat2%*%solve(sigma_EM2)))+
                            sample_hat2 -
                            sample_EM2)
f <- p1+p2
f

s_mu_hat <-  matrix(rep(0, 12*12), ncol=12)
for(i in 1:nrow(simu_comp)){
  n1 <-as.matrix(simu_comp[i,]-mu_hat1)
  s_mu_hat <- s_mu_hat +
    t(n1)%*%n1
}

s_mu_hat

s_mu_hat <- s_mu_hat/nrow(simu_comp)
sample_hat11 <- sum(diag(s_mu_hat%*%solve(sigma_hat1)))

sample_hat11







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

F_total-F_true