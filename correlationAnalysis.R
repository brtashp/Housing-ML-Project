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

# various boxplots:                      
boxplot(startDataTrain$SalePrice ~ startDataTrain$MSSubClass, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$MSZoning, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$Street, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$Alley, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$LotShape, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$LandContour, xlab="Categorical Variable", ylab="Numeric Variable")
boxplot(startDataTrain$SalePrice ~ startDataTrain$Utilities, xlab="Categorical Variable", ylab="Numeric Variable")

# stat version of boxplots
by(startDataTrain$SalePrice, startDataTrain$MSZoning, summary)
by(startDataTrain$SalePrice, startDataTrain$Street, summary)
by(startDataTrain$SalePrice, startDataTrain$Alley, summary)
by(startDataTrain$SalePrice, startDataTrain$LotShape, summary)
by(startDataTrain$SalePrice, startDataTrain$LandContour, summary)
by(startDataTrain$SalePrice, startDataTrain$Utilities, summary)
by(startDataTrain$SalePrice, startDataTrain$LotConfig, summary)


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

# correlation matrix 
correlation_matrix = cor(startDataTrain)
print(correlation_matrix)



# using both datasets: ############################################################

startDataTest = dataTest[, c("MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                               "YrSold", "OverallQual", "OverallCond", "LotFrontage", 
                               "GarageCars", "GarageArea")]

startDataTrain = dataTrain[, c("SalePrice", "MSSubClass", "LotArea", "YearBuilt", "YearRemodAdd", 
                               "YrSold", "OverallQual", "OverallCond", 
                               "GarageCars", "GarageArea")]

summary(startDataTrain)
summary(startDataTest)

boxplot(startDataTrain)
boxplot(startDataTest)

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
summary(New_startDataTest)

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