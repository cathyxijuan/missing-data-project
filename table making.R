library(dplyr)
library(ggplot2)

data <- read.csv("data_study1.csv")
colnames(data)[2] <- "FminB"

data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
                   FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)



data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)

data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="Two Correlated Residuals"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)






####!!!!!!!!!!!!!!!!!!!!!!!11111
data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)



data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="  MCAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism==" Weak MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)

data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.4" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Different Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


data %>% 
  filter(Number_Of_CR=="One Correlated Residual"& Missing_Mechanism=="Strong MAR" & 
           FC=="Factor Correlation = 0.8" & Location_Of_Misfit=="Same Factor") %>% 
  select(c( "RMSEA", "CFI", "Fmin", "FminB")) %>% print(row.names=F)


########For complete data only 
table.complete1 <- data %>% 
  filter(Percent_Missing=="0%" & 
           Number_of_Missing_Variables =="Four Variables with Missing" & 
           Location_Of_Misfit=="Same Factor" &FC=="Factor Correlation = 0" ) %>% 
  select(c( "RMSEA", "CFI", "Number_Of_CR", "Missing_Mechanism", "FC")) %>% print(row.names=F)


#write.csv(table.complete1, "table.complete1.csv", row.names = F)

table.complete2 <- data %>% 
  filter(Percent_Missing=="0%" & 
           Number_of_Missing_Variables =="Four Variables with Missing" & 
           Location_Of_Misfit=="Same Factor" &FC=="Factor Correlation = 0.4" ) %>% 
  select(c( "RMSEA", "CFI", "Number_Of_CR", "Missing_Mechanism", "FC")) %>% print(row.names=F)


#write.csv(table.complete2, "table.complete2.csv", row.names = F)


table.complete3 <- data %>% 
  filter(Percent_Missing=="0%" & 
           Number_of_Missing_Variables =="Four Variables with Missing" & 
           Location_Of_Misfit=="Same Factor" &FC=="Factor Correlation = 0.8" ) %>% 
  select(c( "RMSEA", "CFI", "Number_Of_CR", "Missing_Mechanism", "FC")) %>% print(row.names=F)


#write.csv(table.complete3, "table.complete3.csv", row.names = F)






### Study 2 complete data
data2 <- read.csv("data_study2.csv")
table.complete4<- data2 %>% filter(Percent_Missing=="0%" & 
                                     Number_of_Missing_Variables =="Six Variables with Missing" &
                                     Number_Of_Patterns==" Minimum Missing Pattern") %>% 
  select(c("Missing_Mechanism","FC","RMSEA","CFI","Number_Of_Patterns"))
write.csv(table.complete4, "table.complete4.csv", row.names = F)



table.complete5<- data2 %>% filter(Percent_Missing=="0%" & 
                                     Number_of_Missing_Variables =="Six Variables with Missing" &
                                     Number_Of_Patterns=="Maximum Missing Pattern") %>% 
  select(c("Missing_Mechanism","FC","RMSEA","CFI","Number_Of_Patterns"))
write.csv(table.complete5, "table.complete5.csv", row.names = F)
