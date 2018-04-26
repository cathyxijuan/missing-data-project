library(ggplot2)
library(gridExtra)
source("results.R")


#Min Pattern, Two Variable missing

#One CR 

mc0 <- fitNoMissingShort_WM1[[1]][1:5,]
mc20 <- fitMCAR_Short_WM1[[3]][1:5,]
mc50 <- fitMCAR_Short_WM1[[4]][1:5,]

weak0 <- fitNoMissingShort_WM1[[1]][1:5,]
weak20 <- fitMAR_Linear_Short_WM1[[1]][1:5,]
weak50 <- fitMAR_Linear_Short_WM1[[2]][1:5,]

strong0 <- fitNoMissingShort_WM1[[1]][1:5,]
strong20 <- fitMAR_Linear_Short_WM1[[3]][1:5,]
strong50 <- fitMAR_Linear_Short_WM1[[4]][1:5,]




data <- rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )

perMiss <-rep( rep(c("0%", "20%", "50%"),each=5), 3)
data <- as.data.frame(data, row.names = 1:nrow(data))
data$perMiss<- perMiss 
strength <- rep(c("  Zero Strength (MCAR)", " Weak Strength (MAR)", "Strong Strength (MAR)"), each=15)
model <- rep(0:4, 9)
data$Strength<- strength
data$Model <- model
data$NumCR <- rep(" Two Variable with Missing", nrow(data))


#four


mc0 <- fitNoMissingShort_WM1[[1]][1:5,]
mc20 <- fitMCAR_Short_WM1[[5]][1:5,]
mc50 <- fitMCAR_Short_WM1[[6]][1:5,]

weak0 <- fitNoMissingShort_WM1[[1]][1:5,]
weak20 <- fitMAR_Linear_Short_WM1[[5]][1:5,]
weak50 <- fitMAR_Linear_Short_WM1[[6]][1:5,]

strong0 <- fitNoMissingShort_WM1[[1]][1:5,]
strong20 <- fitMAR_Linear_Short_WM1[[7]][1:5,]
strong50 <- fitMAR_Linear_Short_WM1[[8]][1:5,]


data2 <-rbind(mc0, mc20, mc50, weak0, weak20, weak50, strong0, strong20, strong50 )

data2 <- as.data.frame(data2, row.names = 1:nrow(data))
data2$perMiss<- perMiss 
data2$Strength<- strength
data2$Model <- model
data2$NumCR <- rep("Four Variables with Missing", nrow(data2))

#combine 
datafinal <-rbind(data, data2)
datafinal$Fmin <- datafinal$fmin

ggplot(datafinal, aes(x=Model, y=Fmin, colour=perMiss)) + geom_line(aes(group=perMiss)) + 
  geom_point()+facet_grid(Strength~NumCR)+ scale_colour_discrete(name="Percent Missing")