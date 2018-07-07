source("results.R")
names <- c("Fmin", "Fmin_B", "CFI", "RMSEA")
Size_Of_CR <- data.frame(Size_Of_CR=rep(c(0, 0.1, 0.2, 0.3, 0.4), 3))
FC <- data.frame(FC=rep(c("Factor Correlation = 0", "Factor Correlation = 0.4", "Factor Correlation = 0.8"), each=5))

###

d1 <- cbind(fitNoMissingShort_CR2_3[[2]], fitNoMissingShort_CR2_3[[1]][,"rmsea"])
colnames(d1) <- names
d1


d1 <- cbind(d1, Size_Of_CR, FC)
d1
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("  MCAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("0%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d1 <- cbind(d1, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d1


#################
d2 <- cbind(fitMCAR_Short_CR2_3[[9]],fitMCAR_Short_CR2_3[[1]][,"rmsea"])
colnames(d2) <- names
d2

d2 <- cbind(d2, Size_Of_CR, FC)
d2
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("  MCAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d2 <- cbind(d2, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d2

data <- rbind(d1, d2)
data



#######
d3 <-  cbind(fitMCAR_Short_CR2_3[[10]],fitMCAR_Short_CR2_3[[2]][,"rmsea"])
colnames(d3) <- names
d3

d3 <- cbind(d3, Size_Of_CR, FC)
d3

Missing_Mechanism <- data.frame(Missing_Mechanism=rep("  MCAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d3 <- cbind(d3, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d3

data <- rbind(data, d3)
d3



#################
d4 <- d1
d4$Number_of_Missing_Variables <-"Four Variables with Missing"
d4
data <- rbind(data, d4)
data


#######
d5 <-  cbind(fitMCAR_Short_CR2_3[[13]],fitMCAR_Short_CR2_3[[5]][,"rmsea"])
colnames(d5) <- names
d5
d5 <- cbind(d5, Size_Of_CR, FC)
d5

Missing_Mechanism <- data.frame(Missing_Mechanism=rep("  MCAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d5 <- cbind(d5, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d5

data <- rbind(data, d5)
data


#########
d6 <-  cbind(fitMCAR_Short_CR2_3[[14]],fitMCAR_Short_CR2_3[[6]][,"rmsea"])
colnames(d6) <- names
d6
d6 <- cbind(d6, Size_Of_CR, FC)
d6

Missing_Mechanism <- data.frame(Missing_Mechanism=rep("  MCAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d6 <- cbind(d6, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d6

data <- rbind(data, d6)
data



################
#################
d7 <- d1
d7$Missing_Mechanism <-" Weak MAR"
d7
data <- rbind(data, d7)
data




################
fitMAR_Linear_Short_CR2_3[9]
fitMAR_Linear_Short_CR2_3[1]



d8 <- cbind(fitMAR_Linear_Short_CR2_3[[9]],fitMAR_Linear_Short_CR2_3[[1]][,"rmsea"])
colnames(d8) <- names
d8

d8 <- cbind(d8, Size_Of_CR, FC)
d8
Missing_Mechanism <- data.frame(Missing_Mechanism=rep(" Weak MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d8 <- cbind(d8, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d8

data <- rbind(data, d8)
tail(data,90)



####
################
fitMAR_Linear_Short_CR2_3[10]
fitMAR_Linear_Short_CR2_3[2]

d9 <- cbind(fitMAR_Linear_Short_CR2_3[[10]],fitMAR_Linear_Short_CR2_3[[2]][,"rmsea"])
colnames(d9) <- names
d9

d9 <- cbind(d9, Size_Of_CR, FC)
d9
Missing_Mechanism <- data.frame(Missing_Mechanism=rep(" Weak MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d9 <- cbind(d9, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d9

data <- rbind(data, d9)
tail(data,90)




##########
d10 <- d7
d10
d10$Number_of_Missing_Variables <-"Four Variables with Missing"
d10
data <- rbind(data, d10)
tail(data,90)



##################

fitMAR_Linear_Short_CR2_3[13]
fitMAR_Linear_Short_CR2_3[5]



d11 <- cbind(fitMAR_Linear_Short_CR2_3[[13]],fitMAR_Linear_Short_CR2_3[[5]][,"rmsea"])
colnames(d11) <- names
d11

d11 <- cbind(d11, Size_Of_CR, FC)
d11
Missing_Mechanism <- data.frame(Missing_Mechanism=rep(" Weak MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d11 <- cbind(d11, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d11

data <- rbind(data, d11)
tail(data,90)


##########
fitMAR_Linear_Short_CR2_3[14]
fitMAR_Linear_Short_CR2_3[6]

d12 <- cbind(fitMAR_Linear_Short_CR2_3[[14]],fitMAR_Linear_Short_CR2_3[[6]][,"rmsea"])
colnames(d12) <- names
d12

d12 <- cbind(d12, Size_Of_CR, FC)
d12
Missing_Mechanism <- data.frame(Missing_Mechanism=rep(" Weak MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d12 <- cbind(d12, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d12

data <- rbind(data, d12)
tail(data,90)







###########
d13 <- d7
d13
d13$Missing_Mechanism <-"Strong MAR"
d13
data <- rbind(data, d13)
tail(data,90)




######
fitMAR_Linear_Short_CR2_3[11]
fitMAR_Linear_Short_CR2_3[3]



d14 <- cbind(fitMAR_Linear_Short_CR2_3[[11]],fitMAR_Linear_Short_CR2_3[[3]][,"rmsea"])
colnames(d14) <- names
d14

d14 <- cbind(d14, Size_Of_CR, FC)
d14
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("Strong MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d14 <- cbind(d14, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d14

data <- rbind(data, d14)
tail(data,90)





##########
fitMAR_Linear_Short_CR2_3[12]
fitMAR_Linear_Short_CR2_3[4]

d15 <- cbind(fitMAR_Linear_Short_CR2_3[[12]],fitMAR_Linear_Short_CR2_3[[4]][,"rmsea"])
colnames(d15) <- names
d15

d15 <- cbind(d15, Size_Of_CR, FC)
d15
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("Strong MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep(" Two Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d15 <- cbind(d15, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d15

data <- rbind(data, d15)
tail(data,90)



###########
d16 <- d13
d16
d16$Number_of_Missing_Variables <-"Four Variables with Missing"
d16
data <- rbind(data, d16)



#############

fitMAR_Linear_Short_CR2_3[15]
fitMAR_Linear_Short_CR2_3[7]

d17 <- cbind(fitMAR_Linear_Short_CR2_3[[15]],fitMAR_Linear_Short_CR2_3[[7]][,"rmsea"])
colnames(d17) <- names
d17

d17 <- cbind(d17, Size_Of_CR, FC)
d17
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("Strong MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("20%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d17 <- cbind(d17, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d17

data <- rbind(data, d17)
tail(data,90)





####
fitMAR_Linear_Short_CR2_3[16]
fitMAR_Linear_Short_CR2_3[8]



d18 <- cbind(fitMAR_Linear_Short_CR2_3[[16]],fitMAR_Linear_Short_CR2_3[[8]][,"rmsea"])
colnames(d18) <- names
d18

d18 <- cbind(d18, Size_Of_CR, FC)
d18
Missing_Mechanism <- data.frame(Missing_Mechanism=rep("Strong MAR",15))
Location_Of_Misfit <- data.frame(Location_Of_Misfit=rep("Same Factor", 15))
Percent_Missing <- data.frame(Percent_Missing=rep("50%", 15))
Number_of_Missing_Variables <- data.frame(Number_of_Missing_Variables=rep("Four Variables with Missing", 15))
Number_Of_CR <- data.frame(Number_Of_CR=rep("Two Correlated Residuals", 15))
d18 <- cbind(d18, Number_Of_CR, Location_Of_Misfit, Percent_Missing, Number_of_Missing_Variables ,Missing_Mechanism)
d18

data <- rbind(data, d18)
tail(data,90)


data4=data


##Combine
data_study1 <- rbind(data1, data2, data3, data4)
data_study1
save(data_study1, file="data_study1.R")
write.csv(data_study1, file = "data_study1.csv", row.names = F)
