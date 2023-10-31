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

startDataTrain = dataTrain[, c("SalePrice", "MSZoning", "LotArea", "YearBuilt", "LotFrontage", 
                               "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea",
                               "LandSlope", "Neighborhood", "Condition1",  
                               "BldgType", "HouseStyle", "Street", "Alley", "LotShape",
                               "LandContour", "Utilities", "LotConfig")]

summary(startDataTrain)

by(startDataTrain$SalePrice, startDataTrain$LandSlope, summary)
by(startDataTrain$SalePrice, startDataTrain$Neighborhood, summary)
by(startDataTrain$SalePrice, startDataTrain$Condition1, summary)
by(startDataTrain$SalePrice, startDataTrain$HouseStyle, summary)
by(startDataTrain$SalePrice, startDataTrain$BldgType, summary)

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

# LandSlope
leveling = c("Gtl" = 1, "Mod" = 2, "Sev" = 3)
startDataTrain$LandSlope = factor(startDataTrain$LandSlope, levels = names(leveling))
startDataTrain$LandSlope = as.integer(startDataTrain$LandSlope)

# Neighborhood
leveling = c("IDOTRR" = 1, "BrDale" = 2, "MeadowV" = 3, "Blueste" = 4, "Sawyer" = 5, 
             "Edwards" = 6, "OldTown" = 7, "NPkVill" = 8, "SWISU" = 9, "NridgHt" = 10, 
             "Mitchel" = 11, "Names" = 12, "NWAmes" = 13, "Gilbert" = 14, "CollgCr" = 15, 
             "Blmngtn" = 16, "SawyerW" = 17, "StoneBr" = 18, "ClearCr" = 19, "Timber" = 20, 
             "Veenker" = 21, "Crawfor" = 22, "Somerst" = 23, "NoRidge" = 24)
startDataTrain$Neighborhood = factor(startDataTrain$Neighborhood, levels = names(leveling))
startDataTrain$Neighborhood = as.integer(startDataTrain$Neighborhood)

# Condition1
leveling = c("Artery" = 1, "RRAe" = 2, "Feedr" = 3, "RRAn" = 4, "RRNe" = 5, 
             "PosN" = 6, "Norm" = 7, "PosA" = 8)
startDataTrain$Condition1 = factor(startDataTrain$Condition1, levels = names(leveling))
startDataTrain$Condition1 = as.integer(startDataTrain$Condition1)

# BldgType
leveling = c("2fmCon" = 1, "Duplex" = 2, "Twnhs" = 3, "TwnhsE" = 4, "1Fam" = 5)
startDataTrain$BldgType = factor(startDataTrain$BldgType, levels = names(leveling))
startDataTrain$BldgType = as.integer(startDataTrain$BldgType)

# HouseStyle
leveling = c("5Unf" = 1, "SFoyer" = 2, "1.5Fin" = 3, "2.5Unf" = 4, "SLvl" = 5, "2Story" = 6, "2.5Fin" = 7)
startDataTrain$HouseStyle = factor(startDataTrain$HouseStyle, levels = names(leveling))
startDataTrain$HouseStyle = as.integer(startDataTrain$HouseStyle)

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

startDataTrain$Neighborhood = ifelse(is.na(startDataTrain$Neighborhood), 
                                    median(startDataTrain$Neighborhood,
                                           na.rm = TRUE),
                                    startDataTrain$Neighborhood)

startDataTrain$Condition1 = ifelse(is.na(startDataTrain$Condition1), 
                                     median(startDataTrain$Condition1,
                                            na.rm = TRUE),
                                     startDataTrain$Condition1)

startDataTrain$HouseStyle = ifelse(is.na(startDataTrain$HouseStyle), 
                                   median(startDataTrain$HouseStyle,
                                          na.rm = TRUE),
                                   startDataTrain$HouseStyle)

startDataTrain$LandSlope = ifelse(is.na(startDataTrain$LandSlope), 
                                   median(startDataTrain$LandSlope,
                                          na.rm = TRUE),
                                   startDataTrain$LandSlope)

# correlation matrix 
correlation_matrix = cor(startDataTrain)
print(correlation_matrix)

# using both datasets: ############################################################

startDataTest = dataTest[, c("MSZoning", "LotArea", "YearBuilt", "LotFrontage", 
                               "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea",
                               "LandSlope", "Neighborhood", "Condition1",  
                               "BldgType", "HouseStyle", "Street", "Alley", "LotShape",
                               "LandContour", "Utilities", "LotConfig")]

# below replaces the NAs with the median 

New_startDataTest = startDataTest

# assigning values to cat variables
# MSZoning
leveling = c("C" = 1, "RM" = 2, "RH" = 3, "RL" = 4, "FV" = 5)
New_startDataTest$MSZoning = factor(New_startDataTest$MSZoning, levels = names(leveling))
New_startDataTest$MSZoning = as.integer(New_startDataTest$MSZoning)

# LandSlope
leveling = c("Gtl" = 1, "Mod" = 2, "Sev" = 3)
New_startDataTest$LandSlope = factor(New_startDataTest$LandSlope, levels = names(leveling))
New_startDataTest$LandSlope = as.integer(New_startDataTest$LandSlope)

# Neighborhood
leveling = c("IDOTRR" = 1, "BrDale" = 2, "MeadowV" = 3, "Blueste" = 4, "Sawyer" = 5, 
             "Edwards" = 6, "OldTown" = 7, "NPkVill" = 8, "SWISU" = 9, "NridgHt" = 10, 
             "Mitchel" = 11, "Names" = 12, "NWAmes" = 13, "Gilbert" = 14, "CollgCr" = 15, 
             "Blmngtn" = 16, "SawyerW" = 17, "StoneBr" = 18, "ClearCr" = 19, "Timber" = 20, 
             "Veenker" = 21, "Crawfor" = 22, "Somerst" = 23, "NoRidge" = 24)
New_startDataTest$Neighborhood = factor(New_startDataTest$Neighborhood, levels = names(leveling))
New_startDataTest$Neighborhood = as.integer(New_startDataTest$Neighborhood)

# Condition1
leveling = c("Artery" = 1, "RRAe" = 2, "Feedr" = 3, "RRAn" = 4, "RRNe" = 5, 
             "PosN" = 6, "Norm" = 7, "PosA" = 8)
New_startDataTest$Condition1 = factor(New_startDataTest$Condition1, levels = names(leveling))
New_startDataTest$Condition1 = as.integer(New_startDataTest$Condition1)

# BldgType
leveling = c("2fmCon" = 1, "Duplex" = 2, "Twnhs" = 3, "TwnhsE" = 4, "1Fam" = 5)
New_startDataTest$BldgType = factor(New_startDataTest$BldgType, levels = names(leveling))
New_startDataTest$BldgType = as.integer(New_startDataTest$BldgType)

# HouseStyle
leveling = c("5Unf" = 1, "SFoyer" = 2, "1.5Fin" = 3, "2.5Unf" = 4, "SLvl" = 5, "2Story" = 6, "2.5Fin" = 7)
New_startDataTest$HouseStyle = factor(New_startDataTest$HouseStyle, levels = names(leveling))
New_startDataTest$HouseStyle = as.integer(New_startDataTest$HouseStyle)

# Street
leveling = c("Grvl" = 1, "Pave" = 2)
New_startDataTest$Street = factor(New_startDataTest$Street, levels = names(leveling))
New_startDataTest$Street = as.integer(New_startDataTest$Street)

# Alley 
New_startDataTest$Alley <- ifelse(is.na(New_startDataTest$Alley), "unknown", New_startDataTest$Alley)
leveling = c("Grvl" = 1, "Pave" = 2, "unknown" = 3)
New_startDataTest$Alley = factor(New_startDataTest$Alley, levels = names(leveling))
New_startDataTest$Alley = as.integer(New_startDataTest$Alley)

# LotShape 
leveling = c("IR4" = 1, "IR1" = 2, "IR3" = 3, "IR2" = 4)
New_startDataTest$LotShape = factor(New_startDataTest$LotShape, levels = names(leveling))
New_startDataTest$LotShape = as.integer(New_startDataTest$LotShape)

# LandContour
leveling = c("Bnk" = 1, "Lvl" = 2, "Low" = 3, "HLS" = 4)
New_startDataTest$LandContour = factor(New_startDataTest$LandContour, levels = names(leveling))
New_startDataTest$LandContour = as.integer(New_startDataTest$LandContour)

# Utilities 
leveling = c("NoSeWa" = 1, "AllPub" = 2)
New_startDataTest$Utilities = factor(New_startDataTest$Utilities, levels = names(leveling))
New_startDataTest$Utilities = as.integer(New_startDataTest$Utilities)

# LotConfig
leveling = c("Inside" = 1, "FR2" = 2, "Corner" = 3, "FR3" = 4, "CulDSac" = 5)
New_startDataTest$LotConfig = factor(New_startDataTest$LotConfig, levels = names(leveling))
New_startDataTest$LotConfig = as.integer(New_startDataTest$LotConfig)

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

New_startDataTest$Condition1 = ifelse(is.na(New_startDataTest$Condition1), 
                                        median(New_startDataTest$Condition1,
                                               na.rm = TRUE),
                                        New_startDataTest$Condition1)

New_startDataTest$LotShape = ifelse(is.na(New_startDataTest$LotShape), 
                                 median(New_startDataTest$LotShape,
                                        na.rm = TRUE),
                                 New_startDataTest$LotShape)

New_startDataTest$Neighborhood = ifelse(is.na(New_startDataTest$Neighborhood), 
                                     median(New_startDataTest$Neighborhood,
                                            na.rm = TRUE),
                                     New_startDataTest$Neighborhood)

New_startDataTest$HouseStyle = ifelse(is.na(New_startDataTest$HouseStyle), 
                                   median(New_startDataTest$HouseStyle,
                                          na.rm = TRUE),
                                   New_startDataTest$HouseStyle)

New_startDataTest$Utilities = ifelse(is.na(New_startDataTest$Utilities), 
                                  median(New_startDataTest$Utilities,
                                         na.rm = TRUE),
                                  New_startDataTest$Utilities)

New_startDataTest$LandSlope = ifelse(is.na(New_startDataTest$LandSlope), 
                                     median(New_startDataTest$LandSlope,
                                            na.rm = TRUE),
                                     New_startDataTest$LandSlope)

New_startDataTest$BldgType = ifelse(is.na(New_startDataTest$BldgType), 
                                     median(New_startDataTest$BldgType,
                                            na.rm = TRUE),
                                     New_startDataTest$BldgType)

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
