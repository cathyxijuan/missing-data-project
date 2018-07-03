library(ggplot2)
library(gridExtra)
library(reshape2)
source("results.R")

#Four variables missing
df0 <- fitNoMissingShort_CR2_1[[2]][1:5,1:2]
df20 <- fitMAR_Linear_Short_CR2_1[[13]][1:5,1:2]
df50 <- fitMAR_Linear_Short_CR2_1[[14]][1:5,1:2]



sv0 <- fitNoMissingShort_CR2_3[[2]][1:5,1:2]
sv20 <-fitMAR_Linear_Short_CR2_3[[13]][1:5,1:2]
sv50 <- fitMAR_Linear_Short_CR2_3[[14]][1:5,1:2]


perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
placeMiss <- rep(c(" Different Factor", " Same Factor"), each=15)
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 6)


data2 <- rbind(df0,df20 ,df50,  sv0, sv20, sv50)
data2 <- as.data.frame(data2, row.names = 1:nrow(data2))

data2$PercentMissing<- perMiss 
data2$placeMiss<- placeMiss
data2$ResidualSize<- ResidualSize
data2 <- melt(data2, measure.var=1:2)
data2$Fmin <- data2$value
data2$ModelHB <- rep(c(" Fitted Model", "Baseline"), each=nrow(data2)/2)


#combine 
datafinal <-data2




ggplot(datafinal, aes(x=ResidualSize, y=Fmin)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~ModelHB)+
  xlab("Size of Correlated Residual (Degree of Misfit)")+
  ggtitle("Weak Strength (MAR)")+  theme(plot.title = element_text(hjust = 0.5))

