# project submission 1

# load in library 
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)

# load data in 
dataTrain = read.csv("train.csv")
dataTest =  read.csv("test.csv")


str(dataTrain)
str(dataTest)



#Clean/separate some parts of the data 

startDataTrain = dataTrain[, c("SalePrice", "MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                     "YrSold", "OverallQual", "OverallCond", "LotFrontage", 
                     "GarageCars", "GarageArea")]

startDataTest = dataTest[, c("MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                               "YrSold", "OverallQual", "OverallCond", "LotFrontage", 
                               "GarageCars", "GarageArea")]

# found that the column LotFrontage is the only column with NAs, so lets remove that 
# column 
# found two NAs in garagecars and garagearea columns in startDataTest dataset
summary(startDataTrain)
summary(startDataTest)
startDataTrain = dataTrain[, c("SalePrice", "MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                               "YrSold", "OverallQual", "OverallCond", 
                               "GarageCars", "GarageArea")]

startDataTest = dataTest[, c("MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                             "YrSold", "OverallQual", "OverallCond", 
                             "GarageCars", "GarageArea")]

boxplot(startDataTrain)
boxplot(startDataTest)

# also seeing a lot of variation with lot area (mostly for startDataTest, some for 
# startDataTrain)
# below replaces the NAs with the median 

New_startDataTest = startDataTest

New_startDataTest$GarageCars = ifelse(is.na(New_startDataTest$GarageCars), 
                      median(New_startDataTest$GarageCars,
                             na.rm = TRUE),
                      New_startDataTest$GarageCars)

summary(New_startDataTest)

New_startDataTest$GarageArea = ifelse(is.na(New_startDataTest$GarageArea), 
                                      median(New_startDataTest$GarageArea,
                                             na.rm = TRUE),
                                      New_startDataTest$GarageArea)
summary(New_startDataTest)

# will most likely need to choose different variables 

startModel = randomForest(SalePrice~., data=startDataTrain, ntree=500)

print(startModel)

predictionStart = predict(startModel, newdata = New_startDataTest)

accuracy = mean(predictionStart == New_startDataTest$SalePrice)

confusion_matrix = table(New_startDataTest, predictionStart)

# Train the Random Forest model
startModel <- randomForest(SalePrice ~ ., data = startDataTrain, ntree = 500)

# Predict sale prices for the test dataset
predictionStart <- predict(startModel, newdata = New_startDataTest)

mae <- mean(abs(predictionStart - startDataTrain$SalePrice))

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predictionStart - startDataTrain$SalePrice)^2))

# Print the MAE and RMSE
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")

# Define a threshold for acceptable error
threshold <- 10000

# Calculate accuracy as the percentage of predictions within the threshold
accuracy <- mean(abs(predictionStart - startDataTrain$SalePrice) < threshold)

# Print accuracy
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")