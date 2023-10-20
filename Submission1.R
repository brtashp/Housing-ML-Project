# project submission 1

# load data in 
data = read.csv("train.csv")

str(data)

# load in library 
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)

# need to split data into train and test: 

startData = data[, c("SalePrice", "MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                     "YrSold", "OverallQual", "OverallCond", "LotFrontage", 
                     "GarageCars", "GarageArea")]

startData$MSSubClass[is.na(startData$MSSubClass)]=mean(startData$MSSubClass[!is.na(startData$MSSubClass)])
startData$LotArea[is.na(startData$LotArea)]=mean(startData$LotArea[!is.na(startData$LotArea)])
startData$YearBuilt[is.na(startData$YearBuilt)]=mean(startData$YearBuilt[!is.na(startData$YearBuilt)])
startData$YearRemodAdd[is.na(startData$YearRemodAdd)]=mean(startData$YearRemodAdd[!is.na(startData$YearRemodAdd)])
startData$YrSold[is.na(startData$YrSold)]=mean(startData$YrSold[!is.na(startData$YrSold)])
startData$OverallQual[is.na(startData$OverallQual)]=mean(startData$OverallQual[!is.na(startData$OverallQual)])
startData$OverallCond[is.na(startData$OverallCond)]=mean(startData$OverallCond[!is.na(startData$OverallCond)])
startData$LotFrontage[is.na(startData$LotFrontage)]=mean(startData$LotFrontage[!is.na(startData$LotFrontage)])
startData$GarageCars[is.na(startData$GarageCars)]=mean(startData$GarageCars[!is.na(startData$GarageCars)])
startData$GarageArea[is.na(startData$GarageArea)]=mean(startData$GarageArea[!is.na(startData$GGarageArea)])

str(startData)

set.seed(1019)
split = sample.split(startData$SalePrice, SplitRatio = 0.75)

startDataTrain = subset(startData, split == TRUE)
startDataTest = subset(startData, split == FALSE)

startModel = randomForest(SalePrice~., data=startDataTrain, ntree=500)

print(startModel)

predictionStart = predict(startModel, newdata = startDataTest)

accuracy = mean(predictionStart == startDataTest$SalePrice)

confusion_matrix = table(startDataTest, predictionsStart)
