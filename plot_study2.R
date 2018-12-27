library(dplyr)
library(ggplot2)
data <- read.csv("data_study2.csv")
data
str(data)
MAR_Weak <- filter( data, Missing_Mechanism==" Weak MAR")
MAR_Weak 

MAR_Strong <- filter( data, Missing_Mechanism=="Strong MAR")
MAR_Strong

MCAR <- filter( data, Missing_Mechanism=="  MCAR")
MCAR


ggplot(MCAR, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("MCAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 

ggplot(MAR_Weak, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Weak MAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 

ggplot(MAR_Strong, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Strong MAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 



ggplot(MCAR, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("MCAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 

ggplot(MAR_Weak, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Weak MAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 

ggplot(MAR_Strong, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Strong MAR")+  theme(plot.title = element_text(hjust = 0.5))+ 
  theme_bw() 




six_var_miss <- data %>% filter(Number_of_Missing_Variables=="Six Variables with Missing Data")



ggplot(six_var_miss, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~~Missing_Mechanism) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  theme_bw() 


ggplot(six_var_miss, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~~Missing_Mechanism) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  theme_bw() 


data %>% filter(Number_of_Missing_Variables=="Six Variables with Missing Data" & 
               Number_Of_Patterns=="Maximum Missing Pattern" &
                 Missing_Mechanism==" Weak MAR" & FC==0.2 )

(0.1915 -0.1391)/ 0.1915
(0.5379-0.6794) / 0.5379


data %>% filter(Number_of_Missing_Variables=="Six Variables with Missing Data" & 
                  Number_Of_Patterns==" Minimum Missing Pattern" &
                  Missing_Mechanism==" Weak MAR" & FC==0.2 )
(0.5379-0.6366) / 0.5379
(0.1915 -0.1541)/ 0.1915


data %>% filter(Number_of_Missing_Variables=="Six Variables with Missing Data" & 
                  Number_Of_Patterns=="Maximum Missing Pattern" &
                  Missing_Mechanism==" Weak MAR" & FC==0.7 )


data %>% filter(Number_of_Missing_Variables=="Six Variables with Missing Data" & 
                  Number_Of_Patterns=="Maximum Missing Pattern" &
                  Missing_Mechanism=="Strong MAR" & FC==0.7 )
