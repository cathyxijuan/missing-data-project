library(ggplot2)
library(gridExtra)
source("results.R")


#zero correlation, min pattern, one CR
#Factor correlation = 0 
df0 <- fitNoMissingShort_CR1_1[[2]][1:5,]
df20 <- fitMCAR_Short_CR1_1[[13]][1:5,]
df50 <- fitMCAR_Short_CR1_1[[14]][1:5,]


sv0 <- fitNoMissingShort_CR1_3[[2]][1:5,]
sv20 <- fitMCAR_Short_CR1_3[[13]][1:5,]
sv50 <- fitMCAR_Short_CR1_3[[14]][1:5,]

data <- rbind(df0,df20 ,df50, sv0, sv20, sv50 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$PercentMissing<- perMiss 
placeMiss <- rep(c("Different Factors", "Same Factor"), each=15)
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 6)
data$PlaceMiss<- placeMiss
data$ResidualSize <- ResidualSize
data$FC <- rep("Factor Correlation = 0", nrow(data))


#Factor correlation = 0.4
df02 <- fitNoMissingShort_CR1_1[[2]][6:10,]
df202 <- fitMCAR_Short_CR1_1[[13]][6:10,]
df502 <- fitMCAR_Short_CR1_1[[14]][6:10,]




sv02 <- fitNoMissingShort_CR1_3[[2]][6:10,]
sv202 <- fitMCAR_Short_CR1_3[[13]][6:10,]
sv502 <- fitMCAR_Short_CR1_3[[14]][6:10,]

data2 <- rbind(df02,df202 ,df502,  sv02, sv202, sv502 )

data2 <- as.data.frame(data2, row.names = 1:nrow(data2))
data2$PercentMissing<- perMiss 
data2$PlaceMiss<- placeMiss
data2$ResidualSize <- ResidualSize

data2$FC <- rep("Factor Correlation = 0.4", nrow(data2))


#Factor Correlation - 0.8

#Factor correlation = 0.8
df02 <- fitNoMissingShort_CR1_1[[2]][11:15,]
df202 <- fitMCAR_Short_CR1_1[[13]][11:15,]
df502 <- fitMCAR_Short_CR1_1[[14]][11:15,]




sv02 <- fitNoMissingShort_CR1_3[[2]][11:15,]
sv202 <- fitMCAR_Short_CR1_3[[13]][11:15,]
sv502 <- fitMCAR_Short_CR1_3[[14]][11:15,]

data3 <- rbind(df02,df202 ,df502,  sv02, sv202, sv502 )

data3 <- as.data.frame(data3, row.names = 1:nrow(data3))
data3$PercentMissing<- perMiss 
data3$PlaceMiss<- placeMiss
data3$ResidualSize <- ResidualSize
data3$FC <- rep("Factor Correlation = 0.8", nrow(data3))

#combine 
datafinal <-rbind(data, data2, data3)
datafinal$CFI <- datafinal$cfi


ggplot(datafinal, aes(x=ResidualSize, y=CFI)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(PlaceMiss~FC)+xlab("Size of Correlated Residual")

