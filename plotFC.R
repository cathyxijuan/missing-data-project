library(ggplot2)
library(gridExtra)
source("results.R")


#zero correlation, min pattern, one CR
#Factor correlation = 0 
df0 <- fitNoMissingShort_CR1_1[[1]][1:5,]
df20 <- fitMCAR_Short_CR1_1[[1]][1:5,]
df50 <- fitMCAR_Short_CR1_1[[2]][1:5,]

sfdv0 <- fitNoMissingShort_CR1_2[[1]][1:5,]
sfdv20 <- fitMCAR_Short_CR1_2[[1]][1:5,]
sfdv50 <- fitMCAR_Short_CR1_2[[2]][1:5,]


sv0 <- fitNoMissingShort_CR1_3[[1]][1:5,]
sv20 <- fitMCAR_Short_CR1_3[[1]][1:5,]
sv50 <- fitMCAR_Short_CR1_3[[2]][1:5,]

data <- rbind(df0,df20 ,df50, sfdv0, sfdv20, sfdv50, sv0, sv20, sv50 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:45)
data$PercentMissing<- perMiss 
placeMiss <- rep(c("Different Factors", "Same Factor, Different Variables ", "Same Variables"), each=15)
model <- rep(0:4, 9)
data$PlaceMiss<- placeMiss
data$Model <- model
data$FC <- rep("Factor Correlation = 0", nrow(data))


#Factor correlation = 0.4
df02 <- fitNoMissingShort_CR1_1[[1]][6:10,]
df202 <- fitMCAR_Short_CR1_1[[1]][6:10,]
df502 <- fitMCAR_Short_CR1_1[[2]][6:10,]

sfdv02 <- fitNoMissingShort_CR1_2[[1]][6:10,]
sfdv202 <- fitMCAR_Short_CR1_2[[1]][6:10,]
sfdv502 <- fitMCAR_Short_CR1_2[[2]][6:10,]


sv02 <- fitNoMissingShort_CR1_3[[1]][6:10,]
sv202 <- fitMCAR_Short_CR1_3[[1]][6:10,]
sv502 <- fitMCAR_Short_CR1_3[[2]][6:10,]

data2 <- rbind(df02,df202 ,df502, sfdv02, sfdv202, sfdv502, sv02, sv202, sv502 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data2 <- as.data.frame(data2, row.names = 1:45)
data2$PercentMissing<- perMiss 
placeMiss <- rep(c("Different Factors", "Same Factor, Different Variables ", "Same Variables"), each=15)
model <- rep(0:4, 9)
data2$PlaceMiss<- placeMiss
data2$Model <- model
data2$FC <- rep("Factor Correlation = 0.4", nrow(data2))


#Factor Correlation - 0.8

#Factor correlation = 0.8
df02 <- fitNoMissingShort_CR1_1[[1]][11:15,]
df202 <- fitMCAR_Short_CR1_1[[1]][11:15,]
df502 <- fitMCAR_Short_CR1_1[[2]][11:15,]

sfdv02 <- fitNoMissingShort_CR1_2[[1]][11:15,]
sfdv202 <- fitMCAR_Short_CR1_2[[1]][11:15,]
sfdv502 <- fitMCAR_Short_CR1_2[[2]][11:15,]


sv02 <- fitNoMissingShort_CR1_3[[1]][11:15,]
sv202 <- fitMCAR_Short_CR1_3[[1]][11:15,]
sv502 <- fitMCAR_Short_CR1_3[[2]][11:15,]

data3 <- rbind(df02,df202 ,df502, sfdv02, sfdv202, sfdv502, sv02, sv202, sv502 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data3 <- as.data.frame(data3, row.names = 1:45)
data3$PercentMissing<- perMiss 
placeMiss <- rep(c("Different Factors", "Same Factor, Different Variables ", "Same Variables"), each=15)
model <- rep(0:4, 9)
data3$PlaceMiss<- placeMiss
data3$Model <- model
data3$FC <- rep("Factor Correlation = 0.8", nrow(data3))

#combine 
datafinal <-rbind(data, data2, data3)
datafinal$Fmin <- datafinal$fmin

ggplot(datafinal, aes(x=Model, y=Fmin, colour=PercentMissing)) + geom_line(aes(group=PercentMissing)) + 
  geom_point()+facet_grid(PlaceMiss~FC)+ scale_colour_discrete(name="Percent Missing")
