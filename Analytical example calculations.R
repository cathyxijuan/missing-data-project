library(tmvtnorm)


sigma.not <- matrix(c(1, 0.49, 0.49, 0, 0, 0,
                      0.49, 1, 0.49, 0, 0, 0,
                      0.49, 0.49, 1, 0, 0, 0, 
                      0,    0,    0, 1, 0.49, 0.49, 
                      0,    0,    0, 0.49, 1, 0.49, 
                      0,   0,    0,  0.49, 0.49, 1), nrow=6)
mu.not <- rep(0, 6)
sigma <- matrix(c(1, 0.89, 0.49, 0, 0, 0,
                 0.89, 1, 0.49, 0, 0, 0,
                 0.49, 0.49, 1, 0, 0, 0, 
                 0,    0,    0, 1, 0.49, 0.49, 
                 0,    0,    0, 0.49, 1, 0.49, 
                 0,   0,    0,  0.49, 0.49, 1), nrow=6)
mu <- rep(0, 6)

p <- 6

df <- 27 

## Complete data
fmin <- log(det(sigma.not%*%solve(sigma))) + sum(diag(sigma%*%solve(sigma.not))) +
       t(mu-mu.not)%*%solve(sigma.not)%*%(mu-mu.not) -p
fmin

rmsea <- sqrt(fmin/df)
rmsea


fmin.base <- log(det(solve(sigma))) + sum(diag(sigma))  -p
fmin.base

cfi <- 1-fmin/fmin.base
cfi


#incomplete data
p.incomp <- 5
sigma.base.incomp <- diag(5)
sigma.incomp <- sigma[1:5, 1:5]
sigma.not.incomp <- sigma.not[1:5, 1:5]
mu.incomp <- mu[1:5]
mu.not.incomp <- mu.not[1:5]
fmin.base.incomp <-0.8*(log(det(solve(sigma))) + sum(diag(sigma))  -p) + 
  (0.2)*(log(det(solve(sigma.incomp))) + sum(diag(sigma.incomp))  -p.incomp)
1-fmin/fmin.base.incomp 


fmin.incomp2 <- 0.8*fmin
sqrt(fmin.incomp2/df)

sigma.incomp2 <- sigma[2:6, 2:6]
fmin.base.incomp2 <-0.8*(log(det(solve(sigma))) + sum(diag(sigma))  -p) + 
  (0.2)*(log(det(solve(sigma.incomp2))) + sum(diag(sigma.incomp2))  -p.incomp)
fmin.base.incomp2
1-fmin.incomp2/fmin.base.incomp2



fmin.incomp2 <- 0.5*fmin
sqrt(fmin.incomp2/df)

sigma.incomp2 <- sigma[2:6, 2:6]
fmin.base.incomp2 <-0.5*(log(det(solve(sigma))) + sum(diag(sigma))  -p) + 
  (0.5)*(log(det(solve(sigma.incomp2))) + sum(diag(sigma.incomp2))  -p.incomp)
fmin.base.incomp2
1-fmin.incomp2/fmin.base.incomp2




#MAR data example
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





#second section examples 
eta_not <- 0.5
psi_not <- 1

fmin2.1<- log(4*eta_not*psi_not)+1/psi_not+1/(2*eta_not)-2
fmin2.1
sqrt(fmin2.1)
sqrt(fmin2.1*0.5)

sqrt(fmin2.1/2)

sqrt(0.5274/2)
