# method 1 (initialize kaggle submissions) 
# load in library 
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)

# load data in 
dataTrain = read.csv("train.csv")
dataTest =  read.csv("test.csv")

#str(dataTrain)
#str(dataTest)

#Clean/separate parts of the data 
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

New_startDataTest$GarageArea = ifelse(is.na(New_startDataTest$GarageArea), 
                                      median(New_startDataTest$GarageArea,
                                             na.rm = TRUE),
                                      New_startDataTest$GarageArea)

# will most likely need to choose different variables 
startModel = randomForest(SalePrice~., data=startDataTrain, ntree=500)
predictionStart = predict(startModel, newdata = New_startDataTest)
accuracy = mean(predictionStart == New_startDataTest$SalePrice)

# Train the Random Forest model
startModel <- randomForest(SalePrice ~ ., data = startDataTrain, ntree = 500)
# Predict sale prices for the test dataset
predictionStart <- predict(startModel, newdata = New_startDataTest)

# results 

# split train data into own train and test to test model 
# split data into train and test (to test model)
set.seed(88)
split = sample.split(startDataTrain$SalePrice, SplitRatio = 0.75)
resultsdataTrain = subset(startDataTrain, split == TRUE)
resultsdataTest = subset(startDataTrain, split == FALSE)

# mae results 
mae = mean(abs(resultsdataTrain$SalePrice - predictionStart))

# mse results
# Assuming y_true and y_pred are your actual and predicted values
mse <- mean((resultsdataTrain$SalePrice - predictionStart)^2)

# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((resultsdataTrain$SalePrice - predictionStart)^2))

# mape results
mape <- mean(abs((resultsdataTrain$SalePrice - predictionStart) / resultsdataTrain$SalePrice)) * 100

# Print results
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")

# Define a threshold for acceptable error
threshold <- 1000

# Calculate accuracy as the percentage of predictions within the threshold
accuracy <- mean(abs(resultsdataTrain$SalePrice - predictionStart) < threshold)

# Print accuracy
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")
