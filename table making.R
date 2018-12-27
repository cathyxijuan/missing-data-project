library(dplyr)
library(ggplot2)

data <- read.csv("data_study1.csv")
data2 <- read.csv("data_study2.csv")

str(data)
colnames(data)[2] <- "FminB"
colnames(data2)[2] <- "FminB"

data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
                   FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)




data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)




data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor (DF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor (SF)") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)












###### Study 2
str(data2)
data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="  MCAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

str(data2)
data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism==" Weak MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


str(data2)
data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables ==" Two Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Four Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns==" Minimum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)

data2 %>% filter(Number_of_Missing_Variables =="Six Variables with Missing Data" &
                   Number_Of_Patterns=="Maximum Missing Pattern"
                 & Missing_Mechanism=="Strong MAR")  %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% round(3) %>% print(row.names=F)


