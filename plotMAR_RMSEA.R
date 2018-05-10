library(ggplot2)
library(gridExtra)
source("results.R")
library(reshape2)

#Min Pattern, Two Variable missing. Correlation is 0. two correlated residual


mc0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
mc20 <- fitMCAR_Short_CR2_3[[5]][1:5,]
mc50 <- fitMCAR_Short_CR2_3[[6]][1:5,]

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
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 9)
data$Strength<- strength
data$ResidualSize <- ResidualSize
data$placeMiss <- rep(" Same Factors", nrow(data))




mc0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
mc20 <- fitMCAR_Short_CR2_1[[5]][1:5,]
mc50 <- fitMCAR_Short_CR2_1[[6]][1:5,]

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
data2$Strength<- strength
data2$ResidualSize <- ResidualSize
data2$placeMiss <- rep(" Different Factor", nrow(data2))





datafinal <-rbind(data, data2)
datafinal$RMSEA <- datafinal$rmsea
datafinal$PercentMissing<- datafinal$perMiss 



ggplot(datafinal, aes(x=ResidualSize, y=RMSEA)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~Strength) +xlab("Size of Correlated Residual")


