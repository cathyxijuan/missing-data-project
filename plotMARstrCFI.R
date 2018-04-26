library(ggplot2)
library(gridExtra)
source("results.R")
library(reshape2)

#Min Pattern, Two Variable missing



mc0 <- fitNoMissingShort_CR2_3[[2]][1:5,]
mc20 <- fitMCAR_Short_CR2_3[[15]][1:5,]
mc50 <- fitMCAR_Short_CR2_3[[16]][1:5,]

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
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data$Strength<- strength
data$Model <- model
data$placeMiss <- rep("Misfit and Missing on Different Factors", nrow(data2))




mc0 <- fitNoMissingShort_CR2_1[[2]][1:5,]
mc20 <- fitMCAR_Short_CR2_1[[15]][1:5,]
mc50 <- fitMCAR_Short_CR2_1[[16]][1:5,]

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
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data2$Strength<- strength
data2$Model <- model
data2$placeMiss <- rep(" Misfit and Missing on the Same Factor", nrow(data2))

data2


datafinal <-rbind(data2, data)

ggplot(datafinal, aes(x=Model, y=CFI, colour=perMiss)) + geom_line(aes(group=perMiss)) + 
  geom_point()+facet_grid(Strength~placeMiss)+ scale_colour_discrete(name="Percent Missing")
