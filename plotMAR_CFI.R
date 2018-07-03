library(ggplot2)
library(gridExtra)
source("results.R")
library(reshape2)

#Min Pattern, four Variable missing


mc0 <- fitNoMissingShort_CR2_3[[2]][1:5,]
mc20 <- fitMCAR_Short_CR2_3[[13]][1:5,]
mc50 <- fitMCAR_Short_CR2_3[[14]][1:5,]

weak0 <- fitNoMissingShort_CR2_3[[2]][1:5,]
weak20 <- fitMAR_Linear_Short_CR2_3[[13]][1:5,]
weak50 <- fitMAR_Linear_Short_CR2_3[[14]][1:5,]

strong0 <- fitNoMissingShort_CR2_3[[2]][1:5,]
strong20 <- fitMAR_Linear_Short_CR2_3[[15]][1:5,]
strong50 <- fitMAR_Linear_Short_CR2_3[[16]][1:5,]


data <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$CFI <- data$cfi
data$perMiss<- perMiss 
strength <- rep(c("  MCAR", " Weak MAR", "Strong MAR"), each=15)
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 9)
data$Strength<- strength
data$ResidualSize <- ResidualSize
data$placeMiss <- rep(" Same Factors", nrow(data))




mc0 <- fitNoMissingShort_CR2_1[[2]][1:5,]
mc20 <- fitMCAR_Short_CR2_1[[13]][1:5,]
mc50 <- fitMCAR_Short_CR2_1[[14]][1:5,]

weak0 <- fitNoMissingShort_CR2_1[[2]][1:5,]
weak20 <- fitMAR_Linear_Short_CR2_1[[13]][1:5,]
weak50 <- fitMAR_Linear_Short_CR2_1[[14]][1:5,]

strong0 <- fitNoMissingShort_CR2_1[[2]][1:5,]
strong20 <- fitMAR_Linear_Short_CR2_1[[15]][1:5,]
strong50 <- fitMAR_Linear_Short_CR2_1[[16]][1:5,]


data2 <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data2 <- as.data.frame(data2, row.names = 1:nrow(data2))
data2$CFI <- data2$cfi
data2$perMiss<- perMiss 
strength <- rep(c("  MCAR", " Weak MAR", "Strong MAR"), each=15)
data2$Strength<- strength
data2$ResidualSize <- ResidualSize
data2$placeMiss <- rep(" Different Factor", nrow(data2))





datafinal <-rbind(data, data2)
datafinal$PercentMissing <- datafinal$perMiss

ggplot(datafinal, aes(x=ResidualSize, y=CFI)) +geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~Strength) +xlab("Size of Correlated Residual (Degree of Misfit)")

