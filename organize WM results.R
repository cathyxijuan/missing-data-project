library(dplyr)
load("data_WM_CFI.RData")
load("data_WM_RMSEA.RData")
data_WM_CFI
nrow(data_WM_CFI)
head(data_WM_RMSEA, 30)
head(data_WM_CFI,30)
names(data_WM_CFI)

p1 <- data_WM_CFI[, c("fmin", "fminB", "CFI")]

p2 <- data_WM_RMSEA[, c("RMSEA", "FC.Size", "PercentMissing", "PatternNumber", "NumMissVar", "Strength")]

data_study2 <- cbind(p1, p2)

colnames(data_study2) <- c("Fmin", "Fmin_B", "CFI", "RMSEA", "FC", 
                       "Percent_Missing", "Number_Of_Patterns", "Number_of_Missing_Variables", "Missing_Mechanism")

data_study2
tail(data_study2,100)



#save(data_study2, file="data_study2.RData")
#write.csv(data_study2, file="data_study2.csv", row.names = F)



