library(ggplot2)
library(gridExtra)
source("results.R")


#Min Pattern, Two Variable missing

#One CR 

tv0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
tv20 <- fitMCAR_Short_CR2_3[[1]][1:5,]
tv50 <- fitMCAR_Short_CR2_3[[2]][1:5,]

fv0 <- fitNoMissingShort_CR2_3[[1]][1:5,]
fv20 <- fitMCAR_Short_CR2_3[[5]][1:5,]
fv50 <- fitMCAR_Short_CR2_3[[6]][1:5,]


data <- rbind(tv0, tv20, tv50, fv0, fv20, fv50 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 2)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
placeMiss <- rep(c(" Two Variables with Missing", "Four Variables with Missing"), each=15)
model <- rep(0:4, 6)
data$PlaceMiss<- placeMiss
data$Model <- model
data$NumCR <- rep(" Min Missing Patterns", nrow(data))


#two CR




tv04 <- fitNoMissingShort_CR2_3[[1]][1:5,]
tv204 <- fitMCAR_Short_CR2_3[[3]][1:5,]
tv504 <- fitMCAR_Short_CR2_3[[4]][1:5,]

fv04 <- fitNoMissingShort_CR2_3[[1]][1:5,]
fv204 <- fitMCAR_Short_CR2_3[[7]][1:5,]
fv504 <- fitMCAR_Short_CR2_3[[8]][1:5,]

data2 <- rbind(tv04, tv204, tv504, fv04, fv204, fv504 )

data2 <- as.data.frame(data2, row.names = 1:nrow(data))
data2$perMiss<- perMiss 
data2$PlaceMiss<- placeMiss
data2$Model <- model
data2$NumCR <- rep("Max Missing Patterns", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$Fmin <- datafinal$fmin

ggplot(datafinal, aes(x=Model, y=Fmin, colour=perMiss)) + geom_line(aes(group=perMiss)) + 
  geom_point()+facet_grid(PlaceMiss~NumCR)+ scale_colour_discrete(name="Percent Missing")