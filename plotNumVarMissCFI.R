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
data$CFI <- data$cfi
data$perMiss<- perMiss 
placeMiss <- rep(c("Misfit and Missing on Different Factors", "Misfit and Missing on Same Factor", "Misfit on Factor Correlation"), each=15)
model <- rep(0:4, 9)
data$placeMiss<- placeMiss
data$Model <- model
head(data)







ggplot(data, aes(x=Model, y=CFI, colour=perMiss)) + geom_line(aes(group=perMiss)) + 
  geom_point()+facet_grid(placeMiss~.)+ scale_colour_discrete(name="Percent Missing")
