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



## Case 1: Complete data

fmin.comp <- log(det(sigma.not%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.not))) +
       t(mu-mu.not)%*%solve(sigma.not)%*%(mu-mu.not) -p
fmin.comp

rmsea.case1 <- sqrt(fmin.comp/df)
rmsea.case1

fmin.comp.b <- log(det(solve(sigma))) + sum(diag(sigma))-p
fmin.comp.b

cfi.case1 <- 1-fmin.comp/fmin.comp.b
cfi.case1


## Case 2: MCAR data; misspecification does not involve variables with missing data
p.pattern1 <- 6 #number of variables in the first pattern
sigma.pattern1 <- sigma #population covariance matrix for the first pattern
mu.pattern1 <- mu #population mean vector for the first pattern
sigma.not.pattern1 <- sigma.not #model-implied covariance matrix for the first pattern
mu.not.pattern1 <- mu.not #model-impled mean vector for the first pattern

p.pattern2 <- 5 #number of variables in the second pattern
sigma.pattern2 <- sigma[1:5, 1:5] #population covariance matrix for the second pattern
mu.pattern2 <- mu[1:5]#population mean vector for the second pattern
sigma.not.pattern2 <- sigma.not[1:5, 1:5] #model-implied covariance matrix for the second pattern
mu.not.pattern2 <- mu.not[1:5] #model-impled mean vector for the second pattern


fmin.mcar.case2 <- 0.8*(log(det(sigma.not.pattern1%*%solve(sigma.pattern1))) + 
                          sum(diag(sigma.pattern1%*%solve(sigma.not.pattern1))) +
                          t(mu.pattern1-mu.not.pattern1)%*%solve(sigma.not.pattern1)%*%(mu.pattern1-mu.not.pattern1) 
                        -p.pattern1) +
  0.2*(log(det(sigma.not.pattern2%*%solve(sigma.pattern2))) + 
         sum(diag(sigma.pattern2%*%solve(sigma.not.pattern2))) +
         t(mu.pattern2-mu.not.pattern2)%*%solve(sigma.not.pattern2)%*%(mu.pattern2-mu.not.pattern2) 
       -p.pattern2)
fmin.mcar.case2

rmsea.case2 <- sqrt(fmin.mcar.case2/df)
rmsea.case2

fmin.mcar.b.case2 <-0.8*(log(det(solve(sigma.pattern1))) + sum(diag(sigma.pattern1))  -p.pattern1) + 
  (0.2)*(log(det(solve(sigma.pattern2))) + sum(diag(sigma.pattern2))  -p.pattern2)

cfi.case2 <- 1-fmin.mcar.case2/fmin.mcar.b.case2 
cfi.case2 




#case 3:  MCAR data; misspecification involves variables with missing data
p.pattern1 <- 6 #number of variables in the first pattern
sigma.pattern1 <- sigma #population covariance matrix for the first pattern
mu.pattern1 <- mu #population mean vector for the first pattern
sigma.not.pattern1 <- sigma.not #model-implied covariance matrix for the first pattern
mu.not.pattern1 <- mu.not #model-impled mean vector for the first pattern

p.pattern2 <- 5 #number of variables in the second pattern
sigma.pattern2 <- sigma[2:6, 2:6] #population covariance matrix for the second pattern
mu.pattern2 <- mu[2:6]#population mean vector for the second pattern
sigma.not.pattern2 <- sigma.not[2:6, 2:6] #model-implied covariance matrix for the second pattern
mu.not.pattern2 <- mu.not[2:6] #model-impled mean vector for the second pattern


fmin.mcar.case3 <- 0.8*(log(det(sigma.not.pattern1%*%solve(sigma.pattern1))) + 
                          sum(diag(sigma.pattern1%*%solve(sigma.not.pattern1))) +
                          t(mu.pattern1-mu.not.pattern1)%*%solve(sigma.not.pattern1)%*%(mu.pattern1-mu.not.pattern1) 
                        -p.pattern1) +
  0.2*(log(det(sigma.not.pattern2%*%solve(sigma.pattern2))) + 
         sum(diag(sigma.pattern2%*%solve(sigma.not.pattern2))) +
         t(mu.pattern2-mu.not.pattern2)%*%solve(sigma.not.pattern2)%*%(mu.pattern2-mu.not.pattern2) 
       -p.pattern2)
fmin.mcar.case3

rmsea.case3 <- sqrt(fmin.mcar.case3/df)
rmsea.case3

fmin.mcar.b.case3 <-0.8*(log(det(solve(sigma.pattern1))) + sum(diag(sigma.pattern1))  -p.pattern1) + 
  (0.2)*(log(det(solve(sigma.pattern2))) + sum(diag(sigma.pattern2))  -p.pattern2)

cfi.case3 <- 1-fmin.mcar.case3/fmin.mcar.b.case3
cfi.case3 









#Case 4: MAR data; misspecification involves variables with missing data
cutoff <- qnorm(0.8)
#Pattern1 : complete
mu1    <- rep(0,6)
lower <- rep(-Inf,6)
upper <- c(Inf, cutoff, rep(Inf, 4))
mu_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tmean
sigma_truc1 <- mtmvnorm(mu1, sigma, lower, upper)$tvar


#Pattern2 : x1 is missing
sigma_t <- sigma[-1,-1]
mu2    <- rep(0, 5)
lower <- c(cutoff, rep(-Inf, 4))
upper <- rep(Inf,5)
mu_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tmean
sigma_truc2 <- mtmvnorm(mu2, sigma_t, lower, upper)$tvar



sigma_not1 <- sigma.not
sigma_not2 <- sigma.not[2:p, 2:p]
sigma1 <- sigma
sigma2 <- sigma[2:p, 2:p]


mu_not1 <- mu.not
mu_not2 <-  mu.not[2:p]


p11 <- log(det(sigma_not1%*%solve(sigma1)))

p12 <- 
  sum(diag((sigma_truc1 + 
                   (mu_truc1-mu_not1)%*%t(mu_truc1-mu_not1))%*%solve(sigma_not1)))

p13 <- 
  sum(diag((sigma_truc1 + 
                   (mu_truc1-mu1)%*%t(mu_truc1-mu1))%*%solve(sigma1)))

F_p1 <- p11+p12-p13
F_p1
p21 <- log(det(sigma_not2%*%solve(sigma2)))

p22 <- sum(diag((sigma_truc2 + (mu_truc2-mu_not2)%*%t(mu_truc2-mu_not2))%*%solve(sigma_not2)))

p23 <- sum(diag((sigma_truc2 + (mu_truc2-mu2)%*%t(mu_truc2-mu2))%*%solve(sigma2)))

F_p2 <- p21+p22-p23
F_p2

F_pop <- 
  0.8*F_p1+0.2*F_p2
F_pop




rmsea.MAR <-sqrt(F_pop/df)
rmsea.MAR



#Fmin for the baseline


p11 <- log(det(solve(sigma1)))

p12 <- 
  sum(diag((sigma_truc1 + 
              (mu_truc1)%*%t(mu_truc1))))

p13 <- 
  sum(diag((sigma_truc1 + 
              (mu_truc1-mu1)%*%t(mu_truc1-mu1))%*%solve(sigma1)))

F_p1 <- p11+p12-p13
F_p1
p21 <- log(det(solve(sigma2)))

p22 <- sum(diag((sigma_truc2 + (mu_truc2)%*%t(mu_truc2))))

p23 <- sum(diag((sigma_truc2 + (mu_truc2-mu2)%*%t(mu_truc2-mu2))%*%solve(sigma2)))

F_p2 <- p21+p22-p23
F_p2

F_pop.baseline <- 
  0.8*F_p1+0.2*F_p2
F_pop.baseline

1-F_pop/F_pop.baseline





