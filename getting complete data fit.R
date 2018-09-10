library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.csv("data_study1.csv")
data2 <- read.csv("data_study2.csv")
data

str(data2)

complete1 <-filter(data, Percent_Missing=="0%",
                   Location_Of_Misfit=="Different Factor (DF)",
                   Number_of_Missing_Variables==" Two Variables with Missing Data", 
                   Missing_Mechanism=="  MCAR")
complete1 <- complete1[,c("RMSEA", "CFI")]
round(complete1,3)

complete2 <- filter(data2, Percent_Missing=="0%", Missing_Mechanism=="  MCAR",
                    Number_of_Missing_Variables==" Two Variables with Missing Data",
                    Number_Of_Patterns==" Minimum Missing Pattern" )
complete2<- complete2[,c("RMSEA", "CFI")]
round(complete2,3)
