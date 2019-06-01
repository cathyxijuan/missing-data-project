library(tmvtnorm)

sigma <- matrix(c(1, 0.89, 0.49, 0, 0, 0,
                  0.89, 1, 0.49, 0, 0, 0,
                  0.49, 0.49, 1, 0, 0, 0, 
                  0,    0,    0, 1, 0.49, 0.49, 
                  0,    0,    0, 0.49, 1, 0.49, 
                  0,   0,    0,  0.49, 0.49, 1), nrow=6) #Population covariance matrix
mu <- rep(0, 6) #Population mean vector

sigma.not <- matrix(c(1, 0.49, 0.49, 0, 0, 0,
                      0.49, 1, 0.49, 0, 0, 0,
                      0.49, 0.49, 1, 0, 0, 0, 
                      0,    0,    0, 1, 0.49, 0.49, 
                      0,    0,    0, 0.49, 1, 0.49, 
                      0,   0,    0,  0.49, 0.49, 1), nrow=6) #Model-implied covariance matrix
mu.not <- rep(0, 6) #Model-implied mean vector

p <- 6 #number of variables

df <- 27  #degrees of freedom=6*7/2+6=27


#Case 5: uncorrelated MAR data; misspecification involves variables with missing data
cutoff <- qnorm(0.8) #0.8416212

mu.pattern1    <- mu #population mean vector for pattern 1
sigma.pattern1 <- sigma #population covariance matrix for pattern 1
mu.not.pattern1 <- mu.not #model-implied mean vector for pattern 1
sigma.not.pattern1 <- sigma.not #model-implied covariance matrix for pattern 1
lower <- rep(-Inf,6)
upper <- c(Inf, Inf, Inf, cutoff, Inf, Inf)
mu.star.pattern1 <- mtmvnorm(mu.pattern1, sigma.pattern1, lower, upper)$tmean #pattern-specific mean vector for pattern 1
sigma.star.pattern1 <- mtmvnorm(mu.pattern1, sigma.pattern1, lower, upper)$tvar #pattern-specific covariance matrix for pattern 1



mu.pattern2 <- mu[2:6] #population mean vector for pattern 2
sigma.pattern2 <- sigma[2:6, 2:6] #population covariance matrix for pattern 2
mu.not.pattern2 <- mu.not[2:6] #model-implied mean vector for pattern 2
sigma.not.pattern2 <- sigma.not[2:6,2:6] #model-implied covariance matrix for pattern 2
lower <- c(-Inf, -Inf, cutoff, -Inf, -Inf)
upper <- rep(Inf,5)
mu.star.pattern2 <- mtmvnorm(mu.pattern2, sigma.pattern2, lower, upper)$tmean #pattern-specific mean vector for pattern 2
sigma.star.pattern2 <- mtmvnorm(mu.pattern2, sigma.pattern2, lower, upper)$tvar #pattern-specific covariance matrix for pattern 2


fmin.mar.case4 <- 0.8*(log(det(sigma.not.pattern1%*%solve(sigma.pattern1)))+ #1.200235
                         sum(diag((sigma.star.pattern1 + 
                                     (mu.star.pattern1-mu.not.pattern1)%*%t(mu.star.pattern1-mu.not.pattern1))%*%solve(sigma.not.pattern1))) - #5.317277
                         sum(diag((sigma.star.pattern1 + 
                                     (mu.star.pattern1-mu.pattern1)%*%t(mu.star.pattern1-mu.pattern1))%*%solve(sigma.pattern1)))) + #5.705473
  0.2*(log(det(sigma.not.pattern2%*%solve(sigma.pattern2)))+
         sum(diag((sigma.star.pattern2 + 
                     (mu.star.pattern2-mu.not.pattern2)%*%t(mu.star.pattern2-mu.not.pattern2))%*%solve(sigma.not.pattern2)))-
         sum(diag((sigma.star.pattern2 + 
                     (mu.star.pattern2-mu.pattern2)%*%t(mu.star.pattern2-mu.pattern2))%*%solve(sigma.pattern2))))

5.317277-5.705473 #-0.388196

fmin.mar.case4


rmsea.mar.case4 <-sqrt(fmin.mar.case4/df)
rmsea.mar.case4



fmin.mar.b.case4 <- 0.8*(log(det(solve(sigma.pattern1)))+sum(diag((sigma.star.pattern1 + (mu.star.pattern1)%*%t(mu.star.pattern1))))- sum(diag((sigma.star.pattern1 + (mu.star.pattern1-mu.pattern1)%*%t(mu.star.pattern1-mu.pattern1))%*%solve(sigma.pattern1))))+
  0.2*(log(det(solve(sigma.pattern2)))+sum(diag((sigma.star.pattern2 + (mu.star.pattern2)%*%t(mu.star.pattern2))))- sum(diag((sigma.star.pattern2 + (mu.star.pattern2-mu.pattern2)%*%t(mu.star.pattern2-mu.pattern2))%*%solve(sigma.pattern2))))


cfi.mar.case4 <- 1-fmin.mar.case4/fmin.mar.b.case4
cfi.mar.case4
