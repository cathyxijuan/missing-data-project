[library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)

source("results.R")

noMiss <- fitNoMissingShort_WM1[[1]]
fitMCAR_Short_WM1[1:12]

FC.Size <- rep(c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2), 18)
PercentMissing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
PatternNumber <- rep(rep(c(" Minimum Missing Pattern", "Maximum Missing Pattern"), each=27),3)
NumMissVar <- rep(c(" Two Variables with Missing", "Four Variables with Missing",
                    "Six Variables with Missing"),each=54)
Strength <- rep("  MCAR", 162)

MCAR <-vector()
for(i in 1:12){
  MCAR<- rbind(MCAR,fitMCAR_Short_WM1[1:12][[i]] )
}
MCAR

MCAR <- rbind(noMiss, MCAR[1:18, ], noMiss, MCAR[19:36, ], noMiss, MCAR[37:54,],
      noMiss, MCAR[55:72, ], noMiss, MCAR[73:90,], noMiss, MCAR[91:108,] )

MCAR <- as.data.frame(MCAR, row.names = 1:nrow(MCAR))
MCAR



MCAR$FC.Size <- FC.Size
MCAR$PercentMissing <- PercentMissing
MCAR$PatternNumber<- PatternNumber
MCAR$NumMissVar <- NumMissVar
MCAR$Strength <- Strength 
MCAR$RMSEA <- MCAR$rmsea



##############
fitMAR_minPat_Short_WM1[1:12]
MAR.min <-vector()
for(i in 1:12){
  MAR.min<- rbind(MAR.min,fitMAR_minPat_Short_WM1[1:12][[i]] )
}
MAR.min

MAR.min <- rbind(noMiss, MAR.min[1:18, ], noMiss, MAR.min[19:36, ], noMiss, MAR.min[37:54,],
                 noMiss, MAR.min[55:72, ], noMiss, MAR.min[73:90,], noMiss, MAR.min[91:108,] )

MAR.min <- as.data.frame(MAR.min, row.names = 1:nrow(MAR.min))
MAR.min


PercentMissing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
Strength <- rep(rep(c(" Weak MAR", "Strong MAR"), each=27),3)
NumMissVar <- rep(c(" Two Variables with Missing", "Four Variables with Missing",
                    "Six Variables with Missing"),each=54)
PatternNumber <- rep(" Minimum Missing Pattern", 162)


MAR.min$FC.Size <- FC.Size
MAR.min$PercentMissing <- PercentMissing
MAR.min$PatternNumber<- PatternNumber
MAR.min$NumMissVar <- NumMissVar
MAR.min$Strength <- Strength 
MAR.min$RMSEA <- MAR.min$rmsea






##################################

fitMAR_maxPat_Short_WM1[1:12]

MAR.max <-vector()
for(i in 1:12){
  MAR.max<- rbind(MAR.max,fitMAR_maxPat_Short_WM1[1:12][[i]] )
}
MAR.max

MAR.max <- rbind(noMiss, MAR.max[1:18, ], noMiss, MAR.max[19:36, ], noMiss, MAR.max[37:54,],
                 noMiss, MAR.max[55:72, ], noMiss, MAR.max[73:90,], noMiss, MAR.max[91:108,] )

MAR.max <- as.data.frame(MAR.max, row.names = 1:nrow(MAR.max))
MAR.max


PercentMissing <- rep( rep(c("0%", "20%", "50%"),each=9), 6)
Strength <- rep(rep(c(" Weak MAR", "Strong MAR"), each=27),3)
NumMissVar <- rep(c(" Two Variables with Missing", "Four Variables with Missing",
                    "Six Variables with Missing"),each=54)
PatternNumber <- rep("Maximum Missing Pattern", 162)


MAR.max$FC.Size <- FC.Size
MAR.max$PercentMissing <- PercentMissing
MAR.max$PatternNumber<- PatternNumber
MAR.max$NumMissVar <- NumMissVar
MAR.max$Strength <- Strength 
MAR.max$RMSEA <- MAR.max$rmsea


data_WM_RMSEA <- rbind(MCAR, MAR.min, MAR.max)
tail(data_WM_RMSEA)
#save(data_WM_RMSEA, file="data_WM_RMSEA.RData")




MAR_Weak <- filter(data_WM_RMSEA, Strength==" Weak Strength (MAR)")
MAR_Weak

MAR_Strong <- filter(data_WM_RMSEA, Strength=="Strong Strength (MAR)")
MAR_Strong

MaxPat <- filter(data_WM_RMSEA, PatternNumber=="Maximum Missing Pattern")
MaxPat

ggplot(MCAR, aes(x=FC.Size, y=RMSEA)) +
  geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+
  facet_grid(PatternNumber~NumMissVar) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("MCAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Weak, aes(x=FC.Size, y=RMSEA)) +
  geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+
  facet_grid(PatternNumber~NumMissVar) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Weak MAR")+  theme(plot.title = element_text(hjust = 0.5))

ggplot(MAR_Strong, aes(x=FC.Size, y=RMSEA)) +
  geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+
  facet_grid(PatternNumber~NumMissVar) +
  xlab("Size of Factor Correlation (Degree of Misfit)")+scale_x_reverse()+
  ggtitle("Strong MAR")+  theme(plot.title = element_text(hjust = 0.5))

MAR_Weak %>% filter(PatternNumber=="Maximum Missing Pattern"& NumMissVar=="Six Variables with Missing" &
                      FC.Size=="0.2")




ggplot(MaxPat, aes(x=FC.Size, y=RMSEA)) +
  geom_line(aes(linetype=PercentMissing, color=PercentMissing)) + 
  geom_point(aes(color=PercentMissing))+
  facet_grid(Strength~NumMissVar) +xlab("Size of Factor Correlation")+scale_x_reverse()


