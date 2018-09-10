library(lavaan)
source("functions.R")
source("Models_WM.R")

sigma_WM <-list()
for(i in 1:length(pop.mod)){
  fit <-cfa(pop.mod[[i]], data=simulateData(pop.mod[[i]], sample.nobs=1000))
  sigma_WM[[i]]<- lavInspect(fit, "cov.ov")
}

sigmaHat_WM <-list()

for(i in 1:length(pop.mod)){
  fit <- cfa(model=fitted.mod, sample.nobs=300, sample.cov=sigma_WM[[i]], mimic="EQS")
  sigmaHat_WM[[i]]<- lavInspect(fit, "cov.ov")
}

save(sigmaHat_WM, file="sigmaHat_WM.RData")
save(sigma_WM, file="sigma_WM.RData")





fitNoMissing_WM <-matrix( nrow = 0, ncol = 12)

for(i in 1:length(pop.mod)){
  simuData <- simulateData(pop.mod[[i]], sample.nobs=1000000,seed=111)
  fit <- cfa(fitted.mod, data=simuData)
  fitNoMissing_WM<- rbind(fitNoMissing_WM,lavInspect(fit, "fit")[c("fmin","rmsea","cfi","srmr","gfi", "df", "chisq", "pvalue", "baseline.chisq", "baseline.df", "rmsea.ci.lower", "rmsea.ci.upper")])
}




rmsea_NoMissing_WM <- rmsea_table(fitNoMissing_WM)
cfi_NoMissing_WM <- cfi_table(fitNoMissing_WM)
fitNoMissingShort_WM <- list(rmsea=rmsea_NoMissing_WM, cfi=cfi_NoMissing_WM )

save(fitNoMissing_WM , file="fitNoMissing_WM.RData")
save(fitNoMissingShort_WM , file="fitNoMissingShort_WM.RData")
