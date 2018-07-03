library(ggplot2)
library(gridExtra)
library(reshape2)

source("results.R")


#Fmin Values
df0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
df20 <- fitMCAR_Short_CR2_1[[1]][1:5,]
df50 <- fitMCAR_Short_CR2_1[[2]][1:5,]


sv0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
sv20 <- fitMCAR_Short_CR2_3[[1]][1:5,]
sv50 <- fitMCAR_Short_CR2_3[[2]][1:5,]



data <- rbind(df0,df20 ,df50,  sv0, sv20, sv50)
data <- as.data.frame(data, row.names = 1:nrow(data))

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
ResidualSize <- rep(c(0, 0.1, 0.2, 0.3, 0.4), 6)
placeMiss <- rep(c(" Different Factor", " Same Factor"), each=15)


data$PercentMissing<- perMiss 
data$placeMiss<- placeMiss
data$ResidualSize <- ResidualSize
data$NumMissVar <- rep(" Two Variables with Missing", nrow(data))
head(data)


#four variable missing
df04 <- fitNoMissingShort_CR2_1[[1]][1:5,]
df204 <- fitMCAR_Short_CR2_1[[5]][1:5,]
df504 <- fitMCAR_Short_CR2_1[[6]][1:5,]


sv04 <- fitNoMissingShort_CR2_3[[1]][1:5,]
sv204 <- fitMCAR_Short_CR2_3[[5]][1:5,]
sv504 <- fitMCAR_Short_CR2_3[[6]][1:5,]


data2 <- rbind(df04,df204 ,df504, sv04, sv204, sv504)

data2 <- as.data.frame(data2, row.names = 1:nrow(data))

data2$PercentMissing<- perMiss 
data2$placeMiss<- placeMiss
data2$ResidualSize <- ResidualSize
data2$NumMissVar <- rep("Four Variables with Missing", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$RMSEA <- datafinal$rmsea






ggplot(datafinal, aes(x=ResidualSize, y=RMSEA)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~NumMissVar) +xlab("Size of Correlated Residual \n (Degree of Misfit)")
