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

#New_startDataTest = startDataTest

New_startDataTest$GarageCars = ifelse(is.na(New_startDataTest$GarageCars), 
                                      median(New_startDataTest$GarageCars,
                                             na.rm = TRUE),
                                      New_startDataTest$GarageCars)

New_startDataTest$GarageArea = ifelse(is.na(New_startDataTest$GarageArea), 
                                      median(New_startDataTest$GarageArea,
                                             na.rm = TRUE),
                                      New_startDataTest$GarageArea)

# Train the Random Forest model
startModel <- randomForest(SalePrice ~ ., data = startDataTrain, ntree = 500)
# Predict sale prices for the test dataset
predictionStart <- predict(startModel, newdata = New_startDataTest)

# to submit to kaggle
IDnum = 1461:2919
MySubmission = data.frame(Id = IDnum, SalePrice = predictionStart)
write.csv(MySubmission, "submission1.csv", row.names=FALSE)

# results ######################################################################

# split train data into own train and test to test model 
# split data into train and test (to test model)
set.seed(88)
split = sample.split(startDataTrain$SalePrice, SplitRatio = 0.75)
resultsdataTrain = subset(startDataTrain, split == TRUE)
resultsdataTest = subset(startDataTrain, split == FALSE)
resultsdataTest$SalePrice = NULL

# rerun model using new test and train data
startModelTest = randomForest(SalePrice ~ ., data = resultsdataTrain, ntree = 500)
predictionTest = predict(startModelTest, newdata = resultsdataTest)

# mae results 
mae = mean(abs(resultsdataTrain$SalePrice - predictionTest))
# mse results
# Assuming y_true and y_pred are your actual and predicted values
mse <- mean((resultsdataTrain$SalePrice - predictionTest)^2)
# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((resultsdataTrain$SalePrice - predictionTest)^2))
# mape results
mape <- mean(abs((resultsdataTrain$SalePrice - predictionTest) / 
                   resultsdataTrain$SalePrice)) * 100
# Define a threshold for acceptable error
threshold <- 10000
# Calculate accuracy as the percentage of predictions within the threshold
accuracy <- mean(abs(resultsdataTrain$SalePrice - predictionTest) < threshold)

# Print results
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")

# charts/graphs 
library(pROC)
roc_curve = roc(resultsdataTrain$SalePrice, predictionStart)
plot(roc_curve, main = "ROC Curve", col = "blue")