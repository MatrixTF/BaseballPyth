#Author: Andy Alexander
#Date Created: 10/29/2016
#Purpose:   (a) Determine the optimal exponent for the Pythagorean Theorem of Baseball
#           (b) Compare updated Pythagorean formula to a simple linear regression of run differential
#Original Pytahagorean Formula (created by Bill James): Win% = (Runs Scored)^2/[(Runs Scored)^2 + (Runs Allowed)^2]
#Data: Lahman files for all Major League Baseball games from 1871-2015
#https://www.kaggle.com/seanlahman/the-history-of-baseball
#Goal: Determine x by least squares analysis for: ((RS)^x/[(RS)^x + (RA)^x])*Games - Wins
#*****************************************************************************************

#Delete all objects
rm(list=ls())

#Set the working directory to the location with the unpackaged files downloaded from the above Kaggle site.

#Import Lahman Data
rawteams <- read.csv("team.csv")
allteams <- rawteams[,c("year", "team_id", "g", "w", "l", "r", "ra")]
colnames(allteams) <- c("Year", "Team", "Games", "Wins", "Losses", "RS", "RA")

#Always review the data after importing
#Ties are rare in baseball today but used to be more common
#Let's count a Tie as .5 of a Win and .5 of a Loss
allteams <- within(allteams, {
    Ties <- Games - Wins - Losses
    modWins <- Wins + .5*Ties
    modLosses <- Losses + .5*Ties
    RunDiff <- RS - RA
    WinPerc <- modWins/Games})

#Create function to return least squares value given the range of tested exponents
pyth <- function(data, x){
    datalength <- length(data)
    for(i in 1:length(x)){
        #Add the residual squared values to the dataframe 
        data[,i+datalength] <- (((data$RS^x[i]/(data$RS^x[i] + data$RA^x[i]))*data$Games) - data$modWins)^2
        colnames(data)[i+datalength] <- paste("SSE_", x[i], sep="")
    }
    results <- apply(data[,(1+datalength):length(data)], 2, sum) #sum all of residuals squared
    return(results)
}

#In the 19th century, schedules were unorganized and teams were unbalanced.
#Let's limit the scope to all years after the first World Series in 1903
wsteams <- allteams[allteams$Year >= 1903,]
numexp <- seq(1.6, 2.5, by=.01) #Test exponent values of 1.60 through 2.50 by .01
comp <- pyth(allteams, numexp) #run the function with all team data and the test exponents
minPoint <- c(numexp[which.min(comp)], comp[which.min(comp)])
#Under the least squares approach, the optimal value of x is 1.85.

plot(numexp, comp, 
     main= "Baseball Pythagorean Formula: Least Squares Analysis",
     xlab= "Exponent Value (x): RS^x/(RS^x+RA^x)",
     ylab= "Sum of Residuals Squared")
points(minPoint[1], minPoint[2], col="red", pch=16, cex=1.3)
text(minPoint[1], minPoint[2], "Min Point (x=1.85)", pos=3, cex=.7)

#Nonlinear modeling shows a similar result.
wsexp <- nls(WinPerc ~ I(RS^power/(RS^power+RA^power)), data=wsteams, start = list(power=1))

#The best estimate for x is 1.8485.
#The improved Pythagorean formula is:
#     RS^1.8485
#------------------
#(RS^1.8485 + RA^1.8485)

#**************************************************************************************

#While improved, the modified Pythagorean formula is difficult to interpret.
#Perhaps a linear regression with the derived variable (RS-RA, aka RunDiff) would offer better interpretability.
fit_lm <- lm(WinPerc ~ RunDiff, data= wsteams)

#Results: WinPerc = .0006575(RunDiff) + .5
#Interpretation is much better for the linear formula. A team with 0 run differential would be expected to win 50% of their games. Makes sense.
#To win one more game on expectation, how much should a team improve their run differential?

onemorewin <- (1/162)/fit_lm$coefficients[2] #9.388539
#For one more expected win, a team should increase their run differential by approximately 9.4 runs.
#This insight can drive strategy and offers General Managers a guide when dealing with possible transactions (trades, call ups, lineup manuevers, etc.)  

#How do the two models to predict win percentage compare to one another? Let's use AIC.
aicmodel <- AIC(fit_lm, wsexp)
#While the Pythagorean model is better, it is not overwhelmingly so.
#There is value in having an easier to interpret model.

#Let's compare to the 2016 MLB Standings which were not used to create the models.
library(XML)
cbssports <- "http://www.cbssports.com/mlb/standings/regular"
mlbstandingsraw<-readHTMLTable(cbssports, stringsAsFactors=FALSE)

library(data.table)
standings2016 <- as.data.frame(rbindlist(mlbstandingsraw))
teams2016 <- standings2016[-c(1,7,13,19,25,31),c(1:4,6:8)]
colnames(teams2016) <- c("Team", "Wins", "Losses", "WinPerc", "RS", "RA", "RunDiff")
teams2016[,c(2:7)] = apply(teams2016[,c(2:7)], 2, function(x) as.numeric(as.character(x)))
teams2016[,1] <- gsub(" \\(E\\)|x-|y-", "", teams2016[,1])
teams2016 <- within(teams2016, {
    Games <- Wins + Losses
    PythExpWins <- round(Games * (RS^1.8485/(RS^1.8485 + RA^1.8485)), digits=2)
    LinExpWins <- round(Games * (.000675*RunDiff + .5), digits=2)
    PythDiff <- round((Wins-PythExpWins)^2/PythExpWins, digits = 2)
    LinDiff <- round((Wins-LinExpWins)^2/LinExpWins, digits = 2)
})

chisqsums <- c(sum(teams2016$PythDiff), sum(teams2016$LinDiff))
pvalues <- c(1-pchisq(chisqsums[1], df=30-1), 1-pchisq(chisqsums[2],df=30-1))
rnames <- c("Chi Squared Value", "P-Value")
cnames <- c("Pyth", "Linear")
resultmat <- matrix(c(chisqsums, pvalues), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(rnames, cnames))

#Both p-values are well above .99, meaning there is no indication that both models cannot accurately predict a team's expected number of wins.

#Conclusion: 
#(A) The improved Pythagorean formula to predict a team's win percentage uses an exponent of 1.8485 (or 1.85) instead of 2.
#(B) A linear model that predicts a team's win percentage by run differential is almost as good as the updated Pythagorean formula while offering better interpretability.
