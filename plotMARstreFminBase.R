library(ggplot2)
library(gridExtra)
source("results.R")
library(reshape2)

#Min Pattern, Two Variable missing



mc0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
mc20 <- fitMCAR_Short_CR2_3[[15]][1:5,1:2]
mc50 <- fitMCAR_Short_CR2_3[[16]][1:5,1:2]

weak0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
weak20 <- fitMAR_Linear_Short_CR2_3[[13]][1:5,1:2]
weak50 <- fitMAR_Linear_Short_CR2_3[[14]][1:5,1:2]

strong0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
strong20 <- fitMAR_Linear_Short_CR2_3[[15]][1:5,1:2]
strong50 <- fitMAR_Linear_Short_CR2_3[[16]][1:5,1:2]


data <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data$Strength<- strength
data$Model <- model
data <- melt(data, measure.var=1:2)
data$Fmin <- data$value
data$CFI <- data$cfi
data$ModelHB <- rep(c(" Fitted Model\n (Difference Factors)", 
                      "Baseline Model\n (Difference Factors)"), each=nrow(data)/2)
data


mc0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
mc20 <- fitMCAR_Short_CR2_1[[15]][1:5,1:2]
mc50 <- fitMCAR_Short_CR2_1[[16]][1:5,1:2]

weak0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
weak20 <- fitMAR_Linear_Short_CR2_1[[13]][1:5,1:2]
weak50 <- fitMAR_Linear_Short_CR2_1[[14]][1:5,1:2]

strong0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
strong20 <- fitMAR_Linear_Short_CR2_1[[15]][1:5,1:2]
strong50 <- fitMAR_Linear_Short_CR2_1[[16]][1:5,1:2]


data2 <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )



perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data2 <- as.data.frame(data2, row.names = 1:nrow(data2))
data2$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data2$Strength<- strength
data2$Model <- model
data2 <- melt(data2, measure.var=1:2)
data2$Fmin <- data2$value
data2$CFI <- data2$cfi
data2$ModelHB <- rep(c("   Fitted Model \n (Same Factor)", 
                       "  Baseline Model \n (Same Factor)"), 
                     each=nrow(data2)/2)
data2


datafinal <-rbind(data2, data)
datafinal$PercentMissing <- datafinal$perMiss


ggplot(datafinal, aes(x=Model, y=Fmin)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(Strength~ModelHB)
