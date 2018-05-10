library(ggplot2)
library(gridExtra)
source("results.R")


#zero correlation, min pattern, one CR
#Data with two missing variables FC=0
df0 <- fitNoMissingShort_CR2_1[[1]][1:5,]
df20 <- fitMCAR_Short_CR2_1[[1]][1:5,]
df50 <- fitMCAR_Short_CR2_1[[2]][1:5,]



sv0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
sv20 <- fitMCAR_Short_CR2_3[[1]][1:5,]
sv50 <- fitMCAR_Short_CR2_3[[2]][1:5,]

c0 <- fitNoMissingShort_WM1[[1]]
c20 <- fitMCAR_Short_WM1[[1]]
c50 <- fitMCAR_Short_WM1[[2]]

data <- rbind(df0,df20 ,df50,  sv0, sv20, sv50, c0, c20, c50)

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$PercentMissing<- perMiss 
placeMiss <- rep(c(" Different Factors", " Same Factor", "Connection"), each=15)
model <- rep(0:4, 9)
data$PlaceMiss<- placeMiss
data$Model <- model
data$NumMissVar <- rep(" Two Variables with Missing", nrow(data))


#four variable missing
df04 <- fitNoMissingShort_CR2_1[[1]][1:5,]
df204 <- fitMCAR_Short_CR2_1[[5]][1:5,]
df504 <- fitMCAR_Short_CR2_1[[6]][1:5,]



sv04 <- fitNoMissingShort_CR2_3[[1]][1:5,]
sv204 <- fitMCAR_Short_CR2_3[[5]][1:5,]
sv504 <- fitMCAR_Short_CR2_3[[6]][1:5,]

c04 <- fitNoMissingShort_WM1[[1]]
c204 <- fitMCAR_Short_WM1[[5]]
c504 <- fitMCAR_Short_WM1[[6]]

data2 <- rbind(df04,df204 ,df504, sv04, sv204, sv504, c04, c204, c504)

data2 <- as.data.frame(data2, row.names = 1:nrow(data))
data2$PercentMissing<- perMiss 
data2$PlaceMiss<- placeMiss
data2$Model <- model
data2$NumMissVar <- rep("Four Variables with Missing", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$Fmin <- datafinal$fmin
datafinal$RMSEA <- datafinal$rmsea


ggplot(datafinal, aes(x=Model, y=Fmin)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(PlaceMiss~NumMissVar)

ggplot(datafinal, aes(x=Model, y=RMSEA)) + geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+facet_grid(PlaceMiss~NumMissVar)

