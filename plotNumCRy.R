library(ggplot2)
library(gridExtra)
source("results.R")


#Min Pattern, Two Variable missing

#One CR 
df0 <- fitNoMissingShort_CR1_1[[1]][1:5,]
df20 <- fitMCAR_Short_CR1_1[[1]][1:5,]
df50 <- fitMCAR_Short_CR1_1[[2]][1:5,]



sv0 <- fitNoMissingShort_CR1_3[[1]][1:5,]
sv20 <- fitMCAR_Short_CR1_3[[1]][1:5,]
sv50 <- fitMCAR_Short_CR1_3[[2]][1:5,]

data <- rbind(df0,df20 ,df50, sv0, sv20, sv50 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
placeMiss <- rep(c("Misfit and Missing on Different Factors", "Misfit and Missing on Same Factor"), each=15)
model <- rep(0:4, 6)
data$PlaceMiss<- placeMiss
data$Model <- model
data$NumCR <- rep(" One Correlated Residual", nrow(data))


#two CR
df04 <- fitNoMissingShort_CR2_1[[1]][1:5,]
df204 <- fitMCAR_Short_CR2_1[[1]][1:5,]
df504 <- fitMCAR_Short_CR2_1[[2]][1:5,]



sv04 <- fitNoMissingShort_CR2_3[[1]][1:5,]
sv204 <- fitMCAR_Short_CR2_3[[1]][1:5,]
sv504 <- fitMCAR_Short_CR2_3[[2]][1:5,]

data2 <- rbind(df04,df204 ,df504,sv04, sv204, sv504 )

data2 <- as.data.frame(data2, row.names = 1:nrow(data))
data2$perMiss<- perMiss 
data2$PlaceMiss<- placeMiss
data2$Model <- model
data2$NumCR <- rep("Two Correlated Residuals", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$Fmin <- datafinal$fmin

ggplot(datafinal, aes(x=Model, y=Fmin, colour=perMiss)) + geom_line(aes(group=perMiss)) + 
  geom_point()+facet_grid(PlaceMiss~NumCR)+ scale_colour_discrete(name="Percent Missing")
