library(ggplot2)
library(gridExtra)
library(reshape2)

source("results.R")


#Fmin Values
df0 <- fitNoMissingShort_CR2_1[[2]][1:5,]
df20 <- fitMCAR_Short_CR2_1[[9]][1:5,]
df50 <- fitMCAR_Short_CR2_1[[10]][1:5,]


sv0 <- fitNoMissingShort_CR2_3[[2]][1:5,]
sv20 <- fitMCAR_Short_CR2_3[[9]][1:5,]
sv50 <- fitMCAR_Short_CR2_3[[10]][1:5,]

c0 <- fitNoMissingShort_WM1[[2]][1:5,]
c20 <- fitMCAR_Short_WM1[[9]][1:5,]
c50 <- fitMCAR_Short_WM1[[10]][1:5,]

data <- rbind(df0,df20 ,df50,  sv0, sv20, sv50, c0, c20, c50)
perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
placeMiss <- rep(c(" Different Factors", " Same Factor", "Connection"), each=15)
model <- rep(0:4, 9)
data$placeMiss<- placeMiss
data$Model <- model
data$NumMissVar <- rep(" Two Variables with Missing", nrow(data))
head(data)


#four variable missing
df04 <- fitNoMissingShort_CR2_1[[2]][1:5,]
df204 <- fitMCAR_Short_CR2_1[[13]][1:5,]
df504 <- fitMCAR_Short_CR2_1[[14]][1:5,]


sv04 <- fitNoMissingShort_CR2_3[[2]][1:5,]
sv204 <- fitMCAR_Short_CR2_3[[13]][1:5,]
sv504 <- fitMCAR_Short_CR2_3[[14]][1:5,]

c04 <- fitNoMissingShort_WM1[[2]]
c204 <- fitMCAR_Short_WM1[[13]]
c504 <- fitMCAR_Short_WM1[[14]]

data2 <- rbind(df04,df204 ,df504, sv04, sv204, sv504, c04, c204, c504)

data2 <- as.data.frame(data2, row.names = 1:nrow(data))
data2$perMiss<- perMiss 
data2$placeMiss<- placeMiss
data2$Model <- model
data2$NumMissVar <- rep("Four Variables with Missing", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$CFI <- datafinal$cfi
datafinal$PercentMissing<- datafinal$perMiss 






ggplot(datafinal, aes(x=Model, y=CFI)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(placeMiss~NumMissVar)
