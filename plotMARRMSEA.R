library(ggplot2)
library(gridExtra)
source("results.R")
library(reshape2)

#Min Pattern, Two Variable missing



mc0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
mc20 <- fitMCAR_Short_CR2_3[[7]][1:5,]
mc50 <- fitMCAR_Short_CR2_3[[8]][1:5,]

weak0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
weak20 <- fitMAR_Linear_Short_CR2_3[[5]][1:5,]
weak50 <- fitMAR_Linear_Short_CR2_3[[6]][1:5,]

strong0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
strong20 <- fitMAR_Linear_Short_CR2_3[[7]][1:5,]
strong50 <- fitMAR_Linear_Short_CR2_3[[8]][1:5,]


data <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data$Strength<- strength
data$Model <- model
data$placeMiss <- rep(" Same Factors", nrow(data2))




mc0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
mc20 <- fitMCAR_Short_CR2_1[[1]][1:5,]
mc50 <- fitMCAR_Short_CR2_1[[1]][1:5,]

weak0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
weak20 <- fitMAR_Linear_Short_CR2_1[[5]][1:5,]
weak50 <- fitMAR_Linear_Short_CR2_1[[6]][1:5,]

strong0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
strong20 <- fitMAR_Linear_Short_CR2_1[[7]][1:5,]
strong50 <- fitMAR_Linear_Short_CR2_1[[8]][1:5,]


data2 <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data2 <- as.data.frame(data2, row.names = 1:nrow(data2))
data2$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data2$Strength<- strength
data2$Model <- model
data2$placeMiss <- rep(" Different Factor", nrow(data2))



mc0 <- fitNoMissingShort_WM1[[1]][1:5,]
mc20 <- fitMCAR_Short_WM1[[7]][1:5,]
mc50 <- fitMCAR_Short_WM1[[8]][1:5,]

weak0 <- fitNoMissingShort_WM1[[1]][1:5,]
weak20 <- fitMAR_Linear_Short_WM1[[5]][1:5,]
weak50 <- fitMAR_Linear_Short_WM1[[6]][1:5,]

strong0 <- fitNoMissingShort_WM1[[1]][1:5,]
strong20 <- fitMAR_Linear_Short_WM1[[7]][1:5,]
strong50 <- fitMAR_Linear_Short_WM1[[8]][1:5,]


data3 <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data3 <- as.data.frame(data3, row.names = 1:nrow(data3))
data3$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data3$Strength<- strength
data3$Model <- model
data3$placeMiss <- rep("Connection", nrow(data3))

datafinal <-rbind(data, data2, data3)
datafinal$RMSEA <- datafinal$rmsea
datafinal$PercentMissing<- datafinal$perMiss 

head(data)
head(data3)

ggplot(datafinal, aes(x=Model, y=RMSEA)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(Strength~placeMiss)


