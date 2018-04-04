
# Population covariance matrices for the conditions
load("sigma_CR1_1.RData")
load("sigma_CR1_2.RData")
load("sigma_CR1_3.RData")
load("sigma_CR2_1.RData")
load("sigma_CR2_2.RData")
load("sigma_CR2_3.RData")
load("sigma_WM1.RData")


# Model-implied covariance matrices for no missing data
load("sigmaHat_CR1_1.RData")
load("sigmaHat_CR1_2.RData")
load("sigmaHat_CR1_3.RData")
load("sigmaHat_CR2_1.RData")
load("sigmaHat_CR2_2.RData")
load("sigmaHat_CR2_3.RData")
load("sigmaHat_WM1.RData")

# Fit for no missing data
load("fitNoMissing_CR1_1.RData")
load("fitNoMissing_CR1_2.RData")
load("fitNoMissing_CR1_3.RData")
load("fitNoMissing_CR2_1.RData")
load("fitNoMissing_CR2_2.RData")
load("fitNoMissing_CR2_3.RData")
load("fitNoMissing_WM1.RData")


#Fit for MCAR, min pattern, 2 variables with missing, 20 and 50 percent missing
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_2.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_3.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_1.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_2.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_CR2_3.RData")
load("fitMCAR_MinPat_20PerMiss_2VarMiss_WM1.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_2.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_3.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_1.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_2.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_CR2_3.RData")
load("fitMCAR_MinPat_50PerMiss_2VarMiss_WM1.RData")

#Fit for MCAR, min pattern, 4 variables with missing, 20 and 50 percent missing
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_MinPat_20PerMiss_4VarMiss_WM1.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_MinPat_50PerMiss_4VarMiss_WM1.RData")

#Fit for MCAR, max pattern, 2 variables with missing, 20 and 50 percent missing
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_2.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_3.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_1.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_2.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_CR2_3.RData")
load("fitMCAR_MaxPat_20PerMiss_2VarMiss_WM1.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_2.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_3.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_1.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_2.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_CR2_3.RData")
load("fitMCAR_MaxPat_50PerMiss_2VarMiss_WM1.RData")


# Fit for MCAR, max pattern (same amount of pattern as 2 variable conditions)
# 4 variables with missing, 20 and 50 percent missing
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_MaxPat_20PerMiss_4VarMiss_WM1.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_MaxPat_50PerMiss_4VarMiss_WM1.RData")


# Fit for MCAR, max pattern; 4 variables with missing; 20 and 50 percent missing
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_Max2Pat_20PerMiss_4VarMiss_WM1.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR1_1.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR1_2.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR1_3.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR2_1.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR2_2.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR2_3.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_WM1.RData")
load("fitMCAR_Max2Pat_50PerMiss_4VarMiss_WM1.RData")


#Fit MAR, Strong dependency, 2 variables with missing; 20 and 50 percent missing
load("fitMAR_Strong_20PerMiss_2VarMiss_CR1_1.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_CR1_2.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_CR1_3.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_CR2_1.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_CR2_2.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_CR2_3.RData")
load("fitMAR_Strong_20PerMiss_2VarMiss_WM1.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR1_1.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR1_2.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR1_3.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR2_1.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR2_2.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_CR2_3.RData")
load("fitMAR_Strong_50PerMiss_2VarMiss_WM1.RData")


#Fit MAR, Strong dependency, 4 variables with missing; 20 and 50 percent missing
load("fitMAR_Strong_20PerMiss_4VarMiss_CR1_1.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_CR1_2.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_CR1_3.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_CR2_1.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_CR2_2.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_CR2_3.RData")
load("fitMAR_Strong_20PerMiss_4VarMiss_WM1.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR1_1.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR1_2.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR1_3.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR2_1.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR2_2.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_CR2_3.RData")
load("fitMAR_Strong_50PerMiss_4VarMiss_WM1.RData")


prmatrix(round(fitNoMissing_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MinPat_20PerMiss_2VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MinPat_50PerMiss_2VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MinPat_20PerMiss_4VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MinPat_50PerMiss_4VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MaxPat_20PerMiss_2VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_MaxPat_50PerMiss_2VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_Max2Pat_20PerMiss_4VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))
prmatrix(round(fitMCAR_Max2Pat_50PerMiss_4VarMiss_WM1[5:1,2:3],4), rowlab=rep("",5))





round(fitMCAR_Max2Pat_20PerMiss_4VarMiss_WM1[5:1,2:3],4)
round(fitMCAR_MinPat_50PerMiss_4VarMiss_WM1[5:1,2:3],4)
round(fitMCAR_Max2Pat_50PerMiss_4VarMiss_WM1[5:1,2:3],4)

View(round(fitNoMissing_WM1[5:1,2:3],4))


round(fitNoMissing_CR1_1,4)
round(fitMCAR_MinPat_50PerMiss_4VarMiss_CR1_1,4)
round(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR1_1,4)
round(fitMCAR_Max2Pat_50PerMiss_4VarMiss_WM1,4)

round(fitNoMissing_CR2_3,4)
round(fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_3,4)
round(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_3,4)
round(fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR2_3,4)

round(fitNoMissing_CR2_1,4)
round(fitMCAR_MinPat_50PerMiss_4VarMiss_CR2_1,4)
round(fitMCAR_MaxPat_50PerMiss_4VarMiss_CR2_1,4)
round(fitMCAR_Max2Pat_50PerMiss_4VarMiss_CR2_1,4)





fitNoMissing_WM1
fitMCAR_MinPat_20PerMiss_2VarMiss_WM1
fitMCAR_MinPat_50PerMiss_2VarMiss_WM1
fitMCAR_MaxPat_20PerMiss_2VarMiss_WM1
fitMCAR_MaxPat_50PerMiss_2VarMiss_WM1

fitMCAR_MinPat_20PerMiss_2VarMiss_CR1_1
fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_1
fitMCAR_MaxPat_20PerMiss_2VarMiss_CR1_1
fitMCAR_MaxPat_50PerMiss_2VarMiss_CR1_1


sigma_CR1_1

round(sigmaHat_MCAR_MinPat_50PerMiss_2VarMiss_CR1_3[[4]],4)

sigma_CR1_3[[4]]
round(sigmaHat_CR1_3[[4]],4)
round(sigmaHat_MCAR_MinPat_50PerMiss_2VarMiss_CR1_3[[4]],4)
round(fitMCAR_MinPat_50PerMiss_2VarMiss_CR1_3,4)


fitNoMissing_CR1_1
fitNoMissing_CR1_2
fitNoMissing_CR1_3
fitNoMissing_CR2_1
fitNoMissing_CR2_2
sigma_CR1_1
fitNoMissing_CR1_1
fitMCAR_20Missing_MinPat_CR1_1
fitMCAR_50Missing_MinPat_CR1_1

fitNoMissing_CR1_2
fitMCAR_20Missing_MinPat_CR1_2
fitMCAR_50Missing_MinPat_CR1_2

fitNoMissing_CR1_3
fitMCAR_20Missing_MinPat_CR1_3
fitMCAR_50Missing_MinPat_CR1_3

fitNoMissing_CR2_2

sigma_CR1_1
