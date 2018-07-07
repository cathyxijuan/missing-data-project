library(dplyr)
library(ggplot2)
data <- read.csv("data_study2.csv")
data
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
  ggtitle("MCAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Weak, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Weak MAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Strong, aes(x=FC, y=CFI)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Strong MAR")+  theme(plot.title = element_text(hjust = 0.5))



ggplot(MCAR, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("MCAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Weak, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Weak MAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Strong, aes(x=FC, y=RMSEA)) +
  geom_line(aes(linetype=Percent_Missing, color=Percent_Missing)) + 
  geom_point(aes(color=Percent_Missing))+
  facet_grid(Number_Of_Patterns~Number_of_Missing_Variables) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Strong MAR")+  theme(plot.title = element_text(hjust = 0.5))
