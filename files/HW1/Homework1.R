library(sqldf)
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(OneR)
library(arules)


####TASK1####

football = read_xlsx("C:/Users/Ezgi/Desktop/IE 582/AllDataHW1.xlsx")
noRows_inputData <- nrow(football)

HomeGoalsList <- football$FTHG
mean_HomeGoalsList <- mean(HomeGoalsList)

hist_HomeGoalsLÝst = hist(HomeGoalsList, breaks = 40, main="Histogram for Home Score(goals)", xlab="Home Goals", ylab="Number of Games", lwd = 1, col = "grey", ylim=c(0, 300))
size_HomeGoalsList <- 0:(length(hist_HomeGoalsLÝst)+2)
Pois_HomeGoalsList = (dpois(size_HomeGoalsList, mean_HomeGoalsList))*noRows_inputData
lines(size_HomeGoalsList, Pois_HomeGoalsList, col="red", lwd=2)
legend("topright", legend = c("Histogram", "Poisson Dist."),
       col = c("grey", "red"), lty = 1:1, lwd = 2)

AwayGoalsList <- football$FTAG
mean_AwayGoalsList <- mean(AwayGoalsList)

hist_AwayGoalsLÝst = hist(AwayGoalsList, breaks = 40, main="Histogram for Away Score(goals)", xlab="Away Goals", ylab="Number of Games", lwd = 1, col = "grey", ylim=c(0, 300))
size_AwayGoalsList <- 0:(length(hist_AwayGoalsLÝst)+3)
Pois_AwayGoalsList = (dpois(size_AwayGoalsList, mean_AwayGoalsList))*noRows_inputData
lines(size_AwayGoalsList, Pois_AwayGoalsList, col="red", lwd=2)
legend("topright", legend = c("Histogram", "Poisson Dist."),
       col = c("grey", "red"), lty = 1:1, lwd = 2)

SubHomeFromAway <- HomeGoalsList-AwayGoalsList

hist_SubHomeFromAway = hist(SubHomeFromAway, breaks = 40, main="Histogram for Home Score(goals) - Away Score(goals)", xlab="Home Goals - Away Goals", ylab="Number of Games", lwd = 1, col = "grey")


#####TASK2####

## TASK 2.1

 prob_B365H<- 1/(football$B365H)
 prob_B365D<- 1/(football$B365D)
 prob_B365A<- 1/(football$B365A)
 
 prob_BWH<- 1/(football$BWH)
 prob_BWD<- 1/(football$BWD)
 prob_BWA<- 1/(football$BWA)
 
 prob_IWH<- 1/(football$IWH)
 prob_IWD<- 1/(football$IWD)
 prob_IWA<- 1/(football$IWA)
 
 prob_PSH<- 1/(football$PSH)
 prob_PSD<- 1/(football$PSD)
 prob_PSA<- 1/(football$PSA)
 
 ## TASK 2.2
 
 sum_365 <- (prob_B365H+ prob_B365D+ prob_B365A)
 norm_prob_B365H<- prob_B365H /(sum_365)
 norm_prob_B365D<- prob_B365D /(sum_365)
 norm_prob_B365A<- prob_B365A /(sum_365)
 
 sum_BW <- (prob_BWH+ prob_BWD+ prob_BWA)
 norm_prob_BWH<- prob_BWH /(sum_BW)
 norm_prob_BWD<- prob_BWD /(sum_BW)
 norm_prob_BWA<- prob_BWA /(sum_BW)
 
 sum_IW <- (prob_IWH+ prob_IWD+ prob_IWA)
 norm_prob_IWH<- prob_IWH /(sum_IW)
 norm_prob_IWD<- prob_IWD /(sum_IW)
 norm_prob_IWA<- prob_IWA /(sum_IW)
 
 sum_PS <- (prob_PSH+ prob_PSD+ prob_PSA)
 norm_prob_PSH<- prob_PSH /(sum_PS)
 norm_prob_PSD<- prob_PSD /(sum_PS)
 norm_prob_PSA<- prob_PSA /(sum_PS)

 ## TASK 2.3
 
sub_B365 <-  (prob_B365H - prob_B365A)
sub_BW <- (prob_BWH - prob_BWA)
sub_IW <- (prob_IWH -prob_IWA)
sub_PS <- (prob_PSH - prob_PSA)

#plot(sub_B365,prob_B365D,main="Plot for B365 bookmaker",xlab = "P(home)-P(away)", ylab = "p(draw)")
#plot(sub_BW,prob_BWD,main="Plot for BW bookmaker",xlab = "P(home)-P(away)", ylab = "p(draw)")
#plot(sub_IW,prob_IWD,main="Plot for IW bookmaker",xlab = "P(home)-P(away)", ylab = "p(draw)")
#plot(sub_PS,prob_PSD,main="Plot for PS bookmaker",xlab = "P(home)-P(away)", ylab = "p(draw)")


#disc_B365 <- bin(sub_B365,method = c("length" ),nbins=8)
#disc_BW <- bin(sub_BW,method = c("length" ),nbins=8)
#disc_IW <- bin(sub_IW,method = c("length" ),nbins=8)
#disc_PS <- bin(sub_PS,method = c("length" ),nbins=8)

disc_B365 <- discretize(sub_B365, "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_BW <- discretize(sub_BW, "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_IW <- discretize(sub_IW, "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_PS <- discretize(sub_PS, "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))

bool_draw<-ifelse(football$FTHG==football$FTAG,1,0)

dataForTask2_3 <- as.data.frame(cbind(bool_draw,disc_B365,disc_BW,disc_IW,disc_PS))


drawContainerB365<-vector()
totalContainerB365<-vector()
probDrawB365<-vector()

drawContainerBW<-vector()
totalContainerBW<-vector()
probDrawBW<-vector()

drawContainerIW<-vector()
totalContainerIW<-vector()
probDrawIW<-vector()

drawContainerPS<-vector()
totalContainerPS<-vector()
probDrawPS<-vector()

k=1
for (i in 1:10){
        
drawContainerB365[k] <- sum(filter(dataForTask2_3, disc_B365 == i)$bool_draw)
totalContainerB365[k] <- nrow(filter(dataForTask2_3, disc_B365 == i))
probDrawB365[k] <- drawContainerB365[k]/totalContainerB365[k]

drawContainerBW[k] <- sum(filter(dataForTask2_3, disc_BW == i)$bool_draw)
totalContainerBW[k] <- nrow(filter(dataForTask2_3, disc_BW == i))
probDrawBW[k] <- drawContainerBW[k]/totalContainerBW[k]

drawContainerIW[k] <- sum(filter(dataForTask2_3, disc_IW == i)$bool_draw)
totalContainerIW[k] <- nrow(filter(dataForTask2_3, disc_IW == i))
probDrawIW[k] <- drawContainerIW[k]/totalContainerIW[k]

drawContainerPS[k] <- sum(filter(dataForTask2_3, disc_PS == i)$bool_draw)
totalContainerPS[k] <- nrow(filter(dataForTask2_3, disc_PS == i))
probDrawPS[k] <- drawContainerPS[k]/totalContainerPS[k]


k=k+1
}
 
plot(sub_B365,prob_B365D,main="Plot for B365 bookmaker",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawB365, lwd=5 ,col = "red")
legend("topright", legend = c("Bet365", "Actual Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_BW,prob_BWD,main="Plot for BW bookmaker",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawBW, lwd=5 ,col = "red")
legend("topright", legend = c("BW", "Actual Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_IW,prob_IWD,main="Plot for IW bookmaker",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawIW, lwd=5 ,col = "red")
legend("topright", legend = c("IW", "Actual Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_PS,prob_PSD,main="Plot for PS bookmaker",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawPS, lwd=5 ,col = "red")
legend("topright", legend = c("PS", "Actual Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)


#####TASK3####

## TASK 3

football_bezCzer = filter(football, HR == 0 & AR == 0)
noRows_alteredInputData <- nrow(football_bezCzer)

prob_B365HbezCzer <- 1/(football_bezCzer$B365H)
prob_B365DbezCzer <- 1/(football_bezCzer $B365D)
prob_B365AbezCzer <- 1/(football_bezCzer $B365A)

prob_BWHbezCzer <- 1/(football_bezCzer$BWH)
prob_BWDbezCzer <- 1/(football_bezCzer$BWD)
prob_BWAbezCzer <- 1/(football_bezCzer$BWA)

prob_IWHbezCzer <- 1/(football_bezCzer$IWH)
prob_IWDbezCzer <- 1/(football_bezCzer$IWD)
prob_IWAbezCzer <- 1/(football_bezCzer$IWA)

prob_PSHbezCzer <- 1/(football_bezCzer $PSH)
prob_PSDbezCzer <- 1/(football_bezCzer $PSD)
prob_PSAbezCzer <- 1/(football_bezCzer $PSA)


sub_B365bezCzer <-  (prob_B365HbezCzer  - prob_B365AbezCzer )
sub_BWbezCzer  <- (prob_BWHbezCzer  - prob_BWAbezCzer )
sub_IWbezCzer  <- (prob_IWHbezCzer  -prob_IWAbezCzer )
sub_PSbezCzer  <- (prob_PSHbezCzer  - prob_PSAbezCzer )



disc_B365bezCzer  <- discretize(sub_B365bezCzer , "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_BWbezCzer  <- discretize(sub_BWbezCzer , "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_IWbezCzer  <- discretize(sub_IWbezCzer , "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
disc_PSbezCzer  <- discretize(sub_PSbezCzer , "fixed", breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))

bool_drawbezCzer <-ifelse(football_bezCzer $FTHG==football_bezCzer$FTAG,1,0)

dataForTask3bezCzer  <- as.data.frame(cbind(bool_drawbezCzer ,disc_B365bezCzer ,disc_BWbezCzer ,disc_IWbezCzer ,disc_PSbezCzer ))


drawContainerB365bezCzer <-vector()
totalContainerB365bezCzer <-vector()
probDrawB365bezCzer <-vector()

drawContainerBWbezCzer <-vector()
totalContainerBWbezCzer <-vector()
probDrawBWbezCzer <-vector()

drawContainerIWbezCzer <-vector()
totalContainerIWbezCzer <-vector()
probDrawIWbezCzer <-vector()

drawContainerPSbezCzer <-vector()
totalContainerPSbezCzer <-vector()
probDrawPSbezCzer <-vector()

m=1
for (i in 1:10){
        
        drawContainerB365bezCzer [m] <- sum(filter(dataForTask3bezCzer , disc_B365bezCzer  == i)$bool_drawbezCzer )
        totalContainerB365bezCzer [m] <- nrow(filter(dataForTask3bezCzer , disc_B365bezCzer  == i))
        probDrawB365bezCzer [m] <- drawContainerB365bezCzer [m]/totalContainerB365bezCzer [m]
        
        drawContainerBWbezCzer [m] <- sum(filter(dataForTask3bezCzer , disc_BWbezCzer  == i)$bool_drawbezCzer )
        totalContainerBWbezCzer [m] <- nrow(filter(dataForTask3bezCzer , disc_BWbezCzer  == i))
        probDrawBWbezCzer [m] <- drawContainerBWbezCzer[m]/totalContainerBWbezCzer[m]
        
        drawContainerIWbezCzer [m] <- sum(filter(dataForTask3bezCzer , disc_IWbezCzer  == i)$bool_drawbezCzer )
        totalContainerIWbezCzer [m] <- nrow(filter(dataForTask3bezCzer , disc_IWbezCzer  == i))
        probDrawIWbezCzer [m] <- drawContainerIWbezCzer [m]/totalContainerIWbezCzer [m]
        
        drawContainerPSbezCzer [m] <- sum(filter(dataForTask3bezCzer , disc_PSbezCzer  == i)$bool_drawbezCzer )
        totalContainerPSbezCzer [m] <- nrow(filter(dataForTask3bezCzer , disc_PSbezCzer  == i))
        probDrawPSbezCzer [m] <- drawContainerPSbezCzer [m]/totalContainerPSbezCzer [m]
        
        
        m=m+1
}

plot(sub_B365bezCzer ,prob_B365DbezCzer ,main="Plot for B365 bookmaker without Red Cards",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawB365bezCzer , lwd=5 ,col = "red")
legend("topright", legend = c("Bet365", "Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_BWbezCzer ,prob_BWDbezCzer ,main="Plot for BW bookmaker without Red Cards",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawBWbezCzer , lwd=5 ,col = "red")
legend("topright", legend = c("BW", "Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_IWbezCzer ,prob_IWDbezCzer ,main="Plot for IW bookmaker without Red Cards",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawIWbezCzer , lwd=5 ,col = "red")
legend("topright", legend = c("IW", "Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

plot(sub_PSbezCzer ,prob_PSDbezCzer ,main="Plot for PS bookmaker without Red Cards",xlab = "P(home)-P(away)", ylab = "P(draw)")
points(c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),probDrawPSbezCzer , lwd=5 ,col = "red")
legend("topright", legend = c("PS", "Calculation"),
       col = c("black", "red"), lty = 1:1, lwd = 2)

