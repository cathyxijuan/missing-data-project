library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("data_study1.csv")
data
str(data)

### Location of misfit vs Number of variables with missing. (i.e., Figure 1)
data1 <- filter(data,Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & FC=="Factor Correlation = 0" )
data1


ggplot(data1, aes(x=Size_Of_CR, y=RMSEA)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~Number_of_Missing_Variables) +
  xlab("Size of Correlated Residual \n (Degree of Misfit)")

ggplot(data1, aes(x=Size_Of_CR, y=CFI)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~Number_of_Missing_Variables) +
  xlab("Size of Correlated Residual \n (Degree of Misfit)")


#Location of Misfit and Factor Correlation #i.e., Figure 3

data2 <- filter(data,Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" &Number_of_Missing_Variables=="Four Variables with Missing")
data2

ggplot(data2, aes(x=Size_Of_CR , y=RMSEA)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~FC)+xlab("Size of Correlated Residual \n (Degree of Misfit)")


ggplot(data2, aes(x=Size_Of_CR , y=CFI)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~FC)+xlab("Size of Correlated Residual \n (Degree of Misfit)")





### Location of Misfit vs Missing Mechanism 
data3 <- filter(data,Number_Of_CR=="Two Correlated Residuals" & FC=="Factor Correlation = 0"&Number_of_Missing_Variables=="Four Variables with Missing")
data3

ggplot(data3, aes(x=Size_Of_CR, y=RMSEA)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~Missing_Mechanism) +xlab("Size of Correlated Residual (Degree of Misfit)")



ggplot(data3, aes(x=Size_Of_CR, y=CFI)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~Missing_Mechanism) +xlab("Size of Correlated Residual (Degree of Misfit)")





####Fmin and Fmin_B

data4 <- data %>% filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
                           FC=="Factor Correlation = 0" &  Number_of_Missing_Variables==" Two Variables with Missing")
data4s <- melt(data4, measure.var=1:2)
data4s
tail(data4s, 100)


data4s$ModelHB <- rep(c("   Fitted Model \n (Two Variables with Missing)", 
                      "  Baseline  \n (Two Variables with Missing)"), each=nrow(data4s)/2)

data5 <- filter(data,Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & FC=="Factor Correlation = 0" & 
                  Number_of_Missing_Variables=="Four Variables with Missing")
data5s <- melt(data5, measure.var=1:2)
data5s
tail(data5s, 100)
data5s$ModelHB <- rep(c(" Fitted Model \n (Four Variables With Missing)", 
                       "Baseline  \n (Four Variables with Missing)"), each=nrow(data5s)/2)

data6 <- rbind(data4s, data5s)
data6$Fmin <- data6$value

ggplot(data6, aes(x=Size_Of_CR, y=Fmin)) + geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+facet_grid(Location_Of_Misfit~ModelHB)+xlab("Size of Correlated Residual \n (Degree of Misfit)")
