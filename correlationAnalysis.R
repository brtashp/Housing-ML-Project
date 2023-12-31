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

startDataTrain = dataTrain[, c("SalePrice", "MSZoning", "LotArea", "YearBuilt",  
                               "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea")]

startDataTrain = dataTrain[, c("SalePrice", "LotFrontage", "Street", "Alley", "LotShape", "LandContour", 
                               "Utilities", "LotConfig")]

summary(startDataTrain)

# assigning values to cat variables
# MSZoning
leveling = c("C" = 1, "RM" = 2, "RH" = 3, "RL" = 4, "FV" = 5)
startDataTrain$MSZoning = factor(startDataTrain$MSZoning, levels = names(leveling))
startDataTrain$MSZoning = as.integer(startDataTrain$MSZoning)

# Street
leveling = c("Grvl" = 1, "Pave" = 2)
startDataTrain$Street = factor(startDataTrain$Street, levels = names(leveling))
startDataTrain$Street = as.integer(startDataTrain$Street)

# Alley 
startDataTrain$Alley <- ifelse(is.na(startDataTrain$Alley), "unknown", startDataTrain$Alley)
leveling = c("Grvl" = 1, "Pave" = 2, "unknown" = 3)
startDataTrain$Alley = factor(startDataTrain$Alley, levels = names(leveling))
startDataTrain$Alley = as.integer(startDataTrain$Alley)

# LotShape 
leveling = c("IR4" = 1, "IR1" = 2, "IR3" = 3, "IR2" = 4)
startDataTrain$LotShape = factor(startDataTrain$LotShape, levels = names(leveling))
startDataTrain$LotShape = as.integer(startDataTrain$LotShape)

# LandContour
leveling = c("Bnk" = 1, "Lvl" = 2, "Low" = 3, "HLS" = 4)
startDataTrain$LandContour = factor(startDataTrain$LandContour, levels = names(leveling))
startDataTrain$LandContour = as.integer(startDataTrain$LandContour)

# Utilities 
leveling = c("NoSeWa" = 1, "AllPub" = 2)
startDataTrain$Utilities = factor(startDataTrain$Utilities, levels = names(leveling))
startDataTrain$Utilities = as.integer(startDataTrain$Utilities)

# LotConfig
leveling = c("Inside" = 1, "FR2" = 2, "Corner" = 3, "FR3" = 4, "CulDSac" = 5)
startDataTrain$LotConfig = factor(startDataTrain$LotConfig, levels = names(leveling))
startDataTrain$LotConfig = as.integer(startDataTrain$LotConfig)

# Correcting NAs
startDataTrain$MSZoning = ifelse(is.na(startDataTrain$MSZoning), 
                                      median(startDataTrain$MSZoning,
                                             na.rm = TRUE),
                                      startDataTrain$MSZoning)

startDataTrain$LotFrontage = ifelse(is.na(startDataTrain$LotFrontage), 
                                 median(startDataTrain$LotFrontage,
                                        na.rm = TRUE),
                                 startDataTrain$LotFrontage)

startDataTrain$LotShape = ifelse(is.na(startDataTrain$LotShape), 
                                     median(startDataTrain$LotShape,
                                            na.rm = TRUE),
                                     startDataTrain$LotShape)

startDataTrain$GarageArea = ifelse(is.na(startDataTrain$GarageArea), 
                                      median(startDataTrain$GarageArea,
                                             na.rm = TRUE),
                                   startDataTrain$GarageArea)

startDataTrain$LotFrontage = ifelse(is.na(startDataTrain$LotFrontage), 
                                       median(startDataTrain$LotFrontage,
                                              na.rm = TRUE),
                                    startDataTrain$LotFrontage)

# correlation matrix 
correlation_matrix = cor(startDataTrain)
print(correlation_matrix)



# using both datasets: ############################################################

startDataTrain = dataTrain[, c("SalePrice", "MSZoning", "LotArea", "YearBuilt",  
                             "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea",
                             "LotFrontage")]

startDataTest = dataTest[, c("MSZoning", "LotArea", "YearBuilt",  
                               "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea",
                               "LotFrontage")]

# below replaces the NAs with the median 

New_startDataTest = startDataTest

# assigning values to cat variables
# MSZoning
leveling = c("C" = 1, "RM" = 2, "RH" = 3, "RL" = 4, "FV" = 5)
New_startDataTest$MSZoning = factor(New_startDataTest$MSZoning, levels = names(leveling))
New_startDataTest$MSZoning = as.integer(New_startDataTest$MSZoning)

# Correcting NAs
New_startDataTest$MSZoning = ifelse(is.na(New_startDataTest$MSZoning), 
                                 median(New_startDataTest$MSZoning,
                                        na.rm = TRUE),
                                 New_startDataTest$MSZoning)

New_startDataTest$GarageCars = ifelse(is.na(New_startDataTest$GarageCars), 
                      median(New_startDataTest$GarageCars,
                             na.rm = TRUE),
                      New_startDataTest$GarageCars)

New_startDataTest$GarageArea = ifelse(is.na(New_startDataTest$GarageArea), 
                                      median(New_startDataTest$GarageArea,
                                             na.rm = TRUE),
                                      New_startDataTest$GarageArea)

New_startDataTest$LotFrontage = ifelse(is.na(New_startDataTest$LotFrontage), 
                                      median(New_startDataTest$LotFrontage,
                                             na.rm = TRUE),
                                      New_startDataTest$LotFrontage)

summary(New_startDataTest) # check to make sure there are no more NA's
summary(startDataTrain)


# Train the Random Forest model
startModel = randomForest(SalePrice ~ ., data = startDataTrain, ntree = 500)

# Predict sale prices for the test dataset
predictionStart = predict(startModel, newdata = New_startDataTest)

mae = mean(abs(predictionStart - startDataTrain$SalePrice))

# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((predictionStart - startDataTrain$SalePrice)^2))

# Print the MAE and RMSE
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")

# Define a threshold for acceptable error
threshold = 10000
# Calculate accuracy as the percentage of predictions within the threshold
accuracy = mean(abs(predictionStart - startDataTrain$SalePrice) < threshold)
# Print accuracy
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")
