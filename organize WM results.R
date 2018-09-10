source("results.R")

noMiss <- cbind(fitNoMissingShort_WM[[2]], fitNoMissingShort_WM[[1]][,"rmsea"])
fitMCAR_Short_WM[13:24]

FC<- rep(c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2), 18)
Percent_Missing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
Number_Of_Patterns <- rep(rep(c(" Minimum Missing Pattern", "Maximum Missing Pattern"), each=27),3)
Number_of_Missing_Variables <- rep(c(" Two Variables with Missing Data", "Four Variables with Missing Data",
                                     "Six Variables with Missing Data"),each=54)
Missing_Mechanism <- rep("  MCAR", 162)

MCAR <-vector()
for(i in 0:11){
  
  fit<- cbind(fitMCAR_Short_WM[[13+i]], fitMCAR_Short_WM[[1+i]][,"rmsea"])
  MCAR <- rbind(MCAR,fit)
}
MCAR




MCAR <- rbind(noMiss, MCAR[1:18, ], noMiss, MCAR[19:36, ], noMiss, MCAR[37:54,],
              noMiss, MCAR[55:72, ], noMiss, MCAR[73:90,], noMiss, MCAR[91:108,] )

MCAR <- as.data.frame(MCAR, row.names = 1:nrow(MCAR))
MCAR



colnames(MCAR) <- c("Fmin", "Fmin_B","CFI", "RMSEA")




MCAR$FC <- FC
MCAR$Percent_Missing  <-  Percent_Missing 
MCAR$Number_Of_Patterns<-  Number_Of_Patterns
MCAR$Number_of_Missing_Variables <- Number_of_Missing_Variables
MCAR$Missing_Mechanism <-  Missing_Mechanism 



##############

MAR.min <-vector()
for(i in 0:11){
  fit <- cbind(fitMAR_minPat_Short_WM[[13+i]], fitMAR_minPat_Short_WM[[1+i]][,"rmsea"])
  MAR.min<- rbind(MAR.min,fit )
}
MAR.min

MAR.min <- rbind(noMiss, MAR.min[1:18, ], noMiss, MAR.min[19:36, ], noMiss, MAR.min[37:54,],
                 noMiss, MAR.min[55:72, ], noMiss, MAR.min[73:90,], noMiss, MAR.min[91:108,] )

MAR.min <- as.data.frame(MAR.min, row.names = 1:nrow(MAR.min))
MAR.min


Percent_Missing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
Missing_Mechanism <- rep(rep(c(" Weak MAR", "Strong MAR"), each=27),3)
Number_of_Missing_Variables <- rep(c(" Two Variables with Missing Data", "Four Variables with Missing Data",
                                     "Six Variables with Missing Data"),each=54)
Number_Of_Patterns <- rep(" Minimum Missing Pattern", 162)
colnames(MAR.min) <- c("Fmin", "Fmin_B","CFI", "RMSEA")

MAR.min$FC <- FC
MAR.min$Percent_Missing <- Percent_Missing
MAR.min$Number_Of_Patterns<-  Number_Of_Patterns
MAR.min$Number_of_Missing_Variables <- Number_of_Missing_Variables
MAR.min$Missing_Mechanism <-  Missing_Mechanism 






##################################

fitMAR_maxPat_Short_WM[1:12]

MAR.max <-vector()
for(i in 0:11){
  fit <- cbind(fitMAR_maxPat_Short_WM[[13+i]], fitMAR_maxPat_Short_WM[[1+i]][,"rmsea"])
  MAR.max<- rbind(MAR.max,fit )
}
MAR.max

MAR.max <- rbind(noMiss, MAR.max[1:18, ], noMiss, MAR.max[19:36, ], noMiss, MAR.max[37:54,],
                 noMiss, MAR.max[55:72, ], noMiss, MAR.max[73:90,], noMiss, MAR.max[91:108,] )

MAR.max <- as.data.frame(MAR.max, row.names = 1:nrow(MAR.max))
MAR.max


Percent_Missing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
Missing_Mechanism <- rep(rep(c(" Weak MAR", "Strong MAR"), each=27),3)
Number_of_Missing_Variables <- rep(c(" Two Variables with Missing Data", "Four Variables with Missing Data",
                                     "Six Variables with Missing Data"),each=54)
Number_Of_Patterns <- rep("Maximum Missing Pattern", 162)
colnames(MAR.max) <- c("Fmin", "Fmin_B","CFI", "RMSEA")


MAR.max$FC <- FC
MAR.max$Percent_Missing <- Percent_Missing
MAR.max$Number_Of_Patterns<- Number_Of_Patterns
MAR.max$Number_of_Missing_Variables <- Number_of_Missing_Variables
MAR.max$Missing_Mechanism <- Missing_Mechanism 


data_study2 <- rbind(MCAR, MAR.min, MAR.max)




#save(data_study2, file="data_study2.RData")
#write.csv(data_study2, file="data_study2.csv", row.names = F)



