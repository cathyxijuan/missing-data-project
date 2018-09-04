library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("data_study1.csv")
data

str(data)

##Note: Figure 2 is graphed the last because it is the most complicated

### Location of misfit vs Number of variables with missing. (i.e., Figure 1)
data1 <- filter(data,Number_Of_CR=="Two Correlated Residuals"
                & Missing_Mechanism=="  MCAR" 
                & FC=="Factor Correlation = 0" )
data1


ggplot(data1, aes(x=Size_Of_CR, y=RMSEA)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing)) +
  facet_grid(Location_Of_Misfit~Number_of_Missing_Variables) +
  xlab("Size of Correlated Residual (Degree of Misfit)") +
  scale_y_continuous(limits = c(0, 0.17))+ 
  theme_bw() 

ggplot(data1, aes(x=Size_Of_CR, y=CFI)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing)) +
  facet_grid(Location_Of_Misfit~Number_of_Missing_Variables) +
  xlab("Size of Correlated Residual (Degree of Misfit)")+
  scale_y_continuous(limits = c(0.70, 1))+ 
  theme_bw() 


#Location of Misfit and Factor Correlation #i.e., Figure 3

data2 <- filter(data,Number_Of_CR == "One Correlated Residual"& 
                  Missing_Mechanism =="  MCAR" &
                  Number_of_Missing_Variables=="Four Variables with Missing Data")
data2

filter(data,Number_Of_CR == "One Correlated Residual"& 
         Missing_Mechanism =="  MCAR" &
         Number_of_Missing_Variables=="Four Variables with Missing Data" &
         FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)" & Size_Of_CR=="0.4") 

filter(data,Number_Of_CR == "Two Correlated Residuals"& 
         Missing_Mechanism =="  MCAR" &
         Number_of_Missing_Variables=="Four Variables with Missing Data" &
         FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)" & Size_Of_CR=="0.4") 
(0.1051-0.1022)/0.1051
(0.1749- 0.1736 )/0.1749


ggplot(data2, aes(x=Size_Of_CR , y=RMSEA)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Location_Of_Misfit~FC)+
  xlab("Size of Correlated Residual (Degree of Misfit)") + 
  scale_y_continuous(limits = c(0, 0.17))+ 
  theme_bw() 




ggplot(data2, aes(x=Size_Of_CR , y=CFI)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Location_Of_Misfit~FC)+
  xlab("Size of Correlated Residual (Degree of Misfit)") +
  scale_y_continuous(limits = c(0.70, 1))+ 
  theme_bw() 






### Location of Misfit vs Missing Mechanism #i.e., Figure 4
data3 <- filter(data,Number_Of_CR=="Two Correlated Residuals" & FC=="Factor Correlation = 0.4"
                &Number_of_Missing_Variables=="Four Variables with Missing Data")
data3

filter(data,Number_Of_CR=="Two Correlated Residuals" & FC=="Factor Correlation = 0.4"
       &Number_of_Missing_Variables=="Four Variables with Missing Data" & Missing_Mechanism ==" Weak MAR" 
       & Size_Of_CR=="0.4" & Location_Of_Misfit=="Same Factor (SF)")
(0.1678-0.1350 )/0.1678  
(0.1350- 0.1195) / 0.1350 

filter(data,Number_Of_CR=="Two Correlated Residuals" & FC=="Factor Correlation = 0.4"
       &Number_of_Missing_Variables=="Four Variables with Missing Data" & Missing_Mechanism =="  MCAR" 
       & Size_Of_CR=="0.4" & Location_Of_Misfit=="Same Factor (SF)")
(0.1678-0.1507)/0.1678
(0.1507-.12)/0.1507
filter(data,Number_Of_CR=="Two Correlated Residuals" & FC=="Factor Correlation = 0.4"
       &Number_of_Missing_Variables=="Four Variables with Missing Data" & Missing_Mechanism =="Strong MAR" 
       & Size_Of_CR=="0.4" & Location_Of_Misfit=="Same Factor (SF)")
(0.1678-0.1477)/0.1678
(0.1477-0.1156) /0.1477
ggplot(data3, aes(x=Size_Of_CR, y=RMSEA)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Location_Of_Misfit~Missing_Mechanism) +
  xlab("Size of Correlated Residual (Degree of Misfit)") + 
  scale_y_continuous(limits = c(0, 0.17))+ 
  theme_bw() 




ggplot(data3, aes(x=Size_Of_CR, y=CFI)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Location_Of_Misfit~Missing_Mechanism) +
  xlab("Size of Correlated Residual (Degree of Misfit)")+
  scale_y_continuous(limits = c(0.70, 1))+ 
  theme_bw() 




## Figure 2
####Fmin and Fmin_B

data4 <- data %>% filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
                           FC=="Factor Correlation = 0" &  Number_of_Missing_Variables==" Two Variables with Missing Data")
data4s <- melt(data4, measure.var=1:2)
data4s
tail(data4s, 100)


data4s$ModelHB <- rep(c("   Fitted Model \n (Two Variables \n with Missing Data)", 
                      "  Baseline  \n (Two Variables \n with Missing Data)"), each=nrow(data4s)/2)

data5 <- filter(data,Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & FC=="Factor Correlation = 0" & 
                  Number_of_Missing_Variables=="Four Variables with Missing Data")
data5s <- melt(data5, measure.var=1:2)
data5s
tail(data5s, 100)
data5s$ModelHB <- rep(c(" Fitted Model \n (Four Variables \n with Missing Data)", 
                       "Baseline  \n (Four Variables \n with Missing Data)"), each=nrow(data5s)/2)

data6 <- rbind(data4s, data5s)
data6$Fmin <- data6$value

ggplot(data6, aes(x=Size_Of_CR, y=Fmin)) + 
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Location_Of_Misfit~ModelHB)+
  xlab("Size of Correlated Residual (Degree of Misfit)")+ 
  theme_bw() 

