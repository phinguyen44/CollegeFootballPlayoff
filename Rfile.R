#load data sets. All data found on http://www.sports-reference.com/cfb/
Win <- read.csv("Data.csv")
Ratings <-read.csv("Ratings.csv")
WinPct <- read.csv("WinPct.csv")
ValData <- read.csv("TableFormat2.csv")

#load necessary packages
library(caret)
library(e1071)

#change column names to match one another. This is done just for simplicity's sake
colnames(Win)[2] <- "School"

#view data frames
head(Ratings)
head(WinPct)
head(Win)

#merge data frames into a single set
newData <- merge(Win, WinPct, c("Year", "School"))
newData2 <- merge(newData, Ratings, c("Year", "School"))
newData3 <- merge(newData2, WinPct, by.x = c("Year", "Opponent"), by.y = c("Year", "School"))
newData4 <- merge(newData3, Ratings, by.x = c("Year", "Opponent"), by.y = c("Year", "School"))
head(newData4)

#rearrange variables to complete new data frame for use in analysis
ColData <- newData4[c(1,3,2,4,5,6,7,8,9,10,11,12,13,14)]
Margin.x <- ColData[6]-ColData[7]
Margin.y <- ColData[11]-ColData[12]
ColData[15] <- Margin.x
ColData[16] <- Margin.y
colnames(ColData)[c(15,16)] <- c("Margin.x", "Margin.y")
ColData <- ColData[c(1,2,3,4,5,15,8,9,10,16,13,14)]
head(ColData)

#evaluating individual predictors: 
#first variable: win% (ties are not counted in this case)
winindex <- which(ColData$WinPct.x > ColData$WinPct.y)
test <- ColData[winindex,]
barplot(table(test$Win == 1)/nrow(test),col=c("tomato1", "skyblue1"), ylim = c(0,1), names.arg = c("0", "1"), main = "Accuracy of Win Percentage as Classifier", family = "Helvetica", cex.axis = 0.8)
text(.7, 0.25,sprintf("%.2f%%", 100*(table(test$Win == 1)/nrow(test))[1]))
text(1.9, 0.83, sprintf("%.2f%%", 100*(table(test$Win == 1)/nrow(test))[2]))

#second variable: avg. margin of victory
marginindex <- which(ColData$Margin.x > ColData$Margin.y)
test2 <- ColData[marginindex,]
barplot(table(test2$Win == 1)/nrow(test2),col=c("tomato1", "skyblue1"), ylim = c(0,1), names.arg = c("0", "1"), main = "Accuracy of 'Margin of Victory' as Classifier", family = "Helvetica", cex.axis = 0.8)
text(.7, 0.33,sprintf("%.2f%%", 100*(table(test2$Win == 1)/nrow(test2))[1]))
text(1.9, 0.735, sprintf("%.2f%%", 100*(table(test2$Win == 1)/nrow(test2))[2]))

#third variable: total SRS (sum of team's offensive and defensive SRS)
SRSindex <- which((ColData$OSRS.x+ColData$DSRS.x) > (ColData$OSRS.y+ColData$DSRS.y))
test3 <- ColData[SRSindex,]
barplot(table(test3$Win == 1)/nrow(test3),col=c("tomato1", "skyblue1"), ylim = c(0,1), names.arg = c("0", "1"), main = "Accuracy of SRS as Classifier", family = "Helvetica", cex.axis = 0.8)
text(.7, 0.285,sprintf("%.2f%%", 100*(table(test3$Win == 1)/nrow(test3))[1]))
text(1.9, 0.78, sprintf("%.2f%%", 100*(table(test3$Win == 1)/nrow(test3))[2]))

#train and test set
set <- which(ColData$Year == '2012' | ColData$Year == '2013')
testset <- ColData[set,]
trainset <-ColData[-set,]
dim(trainset)
dim(testset)

#build model
mod <- train(Win~WinPct.x+Margin.x+OSRS.x+DSRS.x+WinPct.y+Margin.y+OSRS.y+DSRS.y, method = 'glm', family = binomial, data = trainset)
finmod <- mod$finalModel
print(finmod)
exp(coef(finmod))

#predict (1 if prob > .5, 0 if prob < .5)
output <- predict(mod, newdata = testset)
pred <- rep(0,length(output))
pred[output>=0.5] <- 1

confusionMatrix(pred,testset$Win)

#run validation data through model to get final probabilities
finaltest <-predict(mod, newdata= ValData)
finaltest

#populate ValData Win%
for (i in 1:6) {
  ValData$Win[i] = finaltest[i]
}