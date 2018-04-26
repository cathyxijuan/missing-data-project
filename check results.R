
# Population covariance matrices for the conditions
load("sigma_CR1_1.RData")
load("sigma_CR1_3.RData")
load("sigma_CR2_1.RData")
load("sigma_CR2_3.RData")
load("sigma_WM1.RData")


# Model-implied covariance matrices for no missing data
load("sigmaHat_CR1_1.RData")
load("sigmaHat_CR1_3.RData")
load("sigmaHat_CR2_1.RData")
load("sigmaHat_CR2_3.RData")
load("sigmaHat_WM1.RData")


# Model-implied covariance matrices for no missing data
load("sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR1_1.RData")
load("sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR1_2.RData")
load("sigmaHat_MCAR_MinPat_20PerMiss_2VarMiss_CR1_3.RData")

# Fit for no missing data
load("fitNoMissing_CR1_1.RData")
load("fitNoMissing_CR1_3.RData")
load("fitNoMissing_CR2_1.RData")
load("fitNoMissing_CR2_3.RData")
load("fitNoMissing_WM1.RData")

# Fit for no missing short 
load("fitNoMissingShort_CR1_1.RData")
load("fitNoMissingShort_CR1_2.RData")
load("fitNoMissingShort_CR1_3.RData")
load("fitNoMissingShort_CR2_1.RData")
load("fitNoMissingShort_CR2_2.RData")
load("fitNoMissingShort_CR2_3.RData")
load("fitNoMissingShort_WM1.RData")


#Fit for MCAR
load("fitMCAR_CR1_1.RData")
load("fitMCAR_CR1_2.RData")
load("fitMCAR_CR1_3.RData")
load("fitMCAR_CR2_1.RData")
load("fitMCAR_CR2_2.RData")
load("fitMCAR_CR2_3.RData")
load("fitMCAR_WM1.RData")
load("fitMCAR_Short_CR1_1.RData")
load("fitMCAR_Short_CR1_2.RData")
load("fitMCAR_Short_CR1_3.RData")
load("fitMCAR_Short_CR2_1.RData")
load("fitMCAR_Short_CR2_2.RData")
load("fitMCAR_Short_CR2_3.RData")
load("fitMCAR_Short_WM1.RData")




#Fit for MAR Linear
load("fitMAR_Linear_Short_CR1_1.RData")
load("fitMAR_Linear_Short_CR1_2.RData")
load("fitMAR_Linear_Short_CR1_3.RData")
load("fitMAR_Linear_Short_CR2_1.RData")
load("fitMAR_Linear_Short_CR2_2.RData")
load("fitMAR_Linear_Short_CR2_3.RData")
load("fitMAR_Linear_Short_WM1.RData")


fitNoMissingShort_CR1_1
fitNoMissingShort_CR1_2
fitNoMissingShort_CR1_3
fitNoMissingShort_CR2_1
fitNoMissingShort_CR2_2
fitNoMissingShort_CR2_3
fitNoMissingShort_WM1

fitNoMissingShort_CR1_1
fitMCAR_Short_CR1_1[1:8]
fitMAR_Linear_Short_CR1_1[1:8]

fitNoMissingShort_CR1_1
Map(function(x) x[11:15,], 
    fitMCAR_Short_CR1_1[9:16])
Map(function(x) x[11:15,], 
    fitMAR_Linear_Short_CR1_1[9:16])

fitNoMissingShort_CR1_3
Map(function(x) x[1:5,], 
    fitMCAR_Short_CR1_3[1:8])


Map(function(x) x[11:15,], 
    fitMAR_Linear_Short_CR1_3[9:16])


fitNoMissingShort_CR1_3
Map(function(x) x[11:15,], fitMCAR_Short_CR1_3[1:8])
Map(function(x) x[11:15,], fitMAR_Linear_Short_CR1_3[1:8])


fitNoMissingShort_WM1
fitMCAR_Short_WM1
fitMAR_Linear_Short_WM1


fitNoMissingShort_CR2_1
Map(function(x) x[11:15,], 
    fitMCAR_Short_CR2_1[9:16])
Map(function(x) x[11:15,], 
    fitMAR_Linear_Short_CR2_1[9:16])

fitNoMissingShort_CR2_2
Map(function(x) x[11:15,], fitMCAR_Short_CR2_2[1:8])
Map(function(x) x[11:15,], fitMAR_Linear_Short_CR2_2[1:8])

fitNoMissingShort_CR2_2
Map(function(x) x[11:15,],
    fitMCAR_Short_CR2_2[9:16])
Map(function(x) x[11:15,],
    fitMAR_Linear_Short_CR2_2[9:16])

fitNoMissingShort_CR2_3
Map(function(x) x[11:15,], 
    fitMCAR_Short_CR2_3[9:16])
Map(function(x) x[11:15,], 
    fitMAR_Linear_Short_CR2_3[9:16])



fitNoMissingShort_CR2_3
Map(function(x) x[11:15,], fitMCAR_Short_CR2_3[1:8])
Map(function(x) x[11:15,], fitMAR_Linear_Short_CR2_3[1:8])



















#investigate EQS
fitNoMissing_CR1_3
sigma_CR1_3
round(fitMCAR_CR1_3[[2]],6)
12*13/2
12+12+1
78-25 #df in missing data
12*13/2+11*10/2
133-25 #df in multiple group 
#But no matter what, Fmin is the same in both case. 

#Truncated data may give you information about misfit on the missing data. But this reasoning may not be right. It is more compatible with the two-stage methods. 
#why did the fit get better? Is it because of the equation? or is it because it has an implied matrix that is closer to the pop matrix?


fitMCAR_CR1_3



