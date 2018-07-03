library(ggplot2)
library(gridExtra)
library(reshape2)

source("results.R")


#Fmin Values
df0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
df20 <- fitMCAR_Short_CR2_1[[9]][1:5,1:2]
df50 <- fitMCAR_Short_CR2_1[[10]][1:5,1:2]



sv0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
sv20 <- fitMCAR_Short_CR2_3[[9]][1:5,1:2]
sv50 <- fitMCAR_Short_CR2_3[[10]][1:5,1:2]



data <- rbind(df0,df20 ,df50,  sv0, sv20, sv50)
data <- as.data.frame(data, row.names = 1:nrow(data))


perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
placeMiss <- rep(c(" Different Factors", " Same Factor"), each=15)
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 6)

data$PercentMissing<- perMiss
data$placeMiss<- placeMiss
data$ResidualSize <- ResidualSize
data <- melt(data, measure.var=1:2)
data$Fmin <- data$value
data$ModelHB <- rep(c("   Fitted Model \n (Two Variables with Missing)", 
                      "  Baseline  \n (Two Variables with Missing)"), each=nrow(data)/2)


#Four variables missing
df0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
df20 <- fitMCAR_Short_CR2_1[[13]][1:5,1:2]
df50 <- fitMCAR_Short_CR2_1[[14]][1:5,1:2]



sv0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
sv20 <- fitMCAR_Short_CR2_3[[13]][1:5,1:2]
sv50 <- fitMCAR_Short_CR2_3[[14]][1:5,1:2]



data2 <- rbind(df0,df20 ,df50,  sv0, sv20, sv50)
data2 <- as.data.frame(data2, row.names = 1:nrow(data))

data2$PercentMissing<- perMiss 
data2$placeMiss<- placeMiss
data2$ResidualSize<- ResidualSize
data2 <- melt(data2, measure.var=1:2)
data2$Fmin <- data2$value
data2$ModelHB <- rep(c(" Fitted Model \n (Four Variables With Missing)", 
                      "Baseline  \n (Four Variables with Missing)"), each=nrow(data)/2)


#combine 
datafinal <-rbind(data, data2)
datafinal$CFI <- datafinal$cfi



ggplot(datafinal, aes(x=ResidualSize, y=Fmin)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~ModelHB)+xlab("Size of Correlated Residual \n (Degree of Misfit)")


