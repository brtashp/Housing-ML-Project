# project submission 1

# load in library 
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
#library(dplyr)

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
                               "LandContour", "Utilities", "LotConfig", "OverallCond",
                               "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType")]

#summary(startDataTrain)

#by(startDataTrain$SalePrice, startDataTrain$LandSlope, summary)
#by(startDataTrain$SalePrice, startDataTrain$Neighborhood, summary)
#by(startDataTrain$SalePrice, startDataTrain$Condition1, summary)
#by(startDataTrain$SalePrice, startDataTrain$HouseStyle, summary)
#by(startDataTrain$SalePrice, startDataTrain$BldgType, summary)
#by(startDataTrain$SalePrice, startDataTrain$OverallCond, summary)
#by(startDataTrain$SalePrice, startDataTrain$YearRemodAdd, summary)
#by(startDataTrain$SalePrice, startDataTrain$Exterior1st, summary)
#by(startDataTrain$SalePrice, startDataTrain$Exterior2nd, summary)
#by(startDataTrain$SalePrice, startDataTrain$MasVnrType, summary)

#boxplot(SalePrice ~ MasVnrArea, data = startDataTrain)

# other code for reassigning values 

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

# OverallCond
custom_mapping <- c(`1` = 1, `2` = 4, `3` = 2, `4` = 3, `5` = 8, `6` = 5, `7` = 7, `8` = 6, `9` = 9)
startDataTrain$OverallCond <- custom_mapping[as.character(startDataTrain$OverallCond)]

# RoofStyle
leveling = c("Gambrel" = 1, "Gable" = 2, "Mansard" = 3, "Flat" = 4, "Hip" = 5, "Shed" = 6)
startDataTrain$RoofStyle = factor(startDataTrain$RoofStyle, levels = names(leveling))
startDataTrain$RoofStyle = as.integer(startDataTrain$RoofStyle)

# RoofMatl
leveling = c("Roll" = 1, "ClyTile" = 2, "CompShg" = 3, "Metal" = 4, "Tar&Grv" = 5, "WdShake" = 6, "Membran" = 7, "WdShngl" = 8)
startDataTrain$RoofMatl = factor(startDataTrain$RoofMatl, levels = names(leveling))
startDataTrain$RoofMatl = as.integer(startDataTrain$RoofMatl)

# Exterior1st 
leveling = c("BrkComm" = 1, "AsphShn" = 2, "CBlock" = 3, "AsbShng" = 4, "MetalSd" = 5, 
             "Wd Sdng" = 6, "WdShing" = 7, "Stucco" = 8, "HdBoard" = 9, "Plywood" = 10,
             "BrkFace" = 11, "VinylSd" = 12, "CemntBd" = 13, "Stone" = 14, "ImStucc" = 15)
startDataTrain$Exterior1st = factor(startDataTrain$Exterior1st, levels = names(leveling))
startDataTrain$Exterior1st = as.integer(startDataTrain$Exterior1st)

# Exterior2nd 
leveling = c("CBlock" = 1, "AsbShng" = 2, "Brk Cmn" = 3, "AsphShn" = 4, "Wd Sdng" = 5, 
             "MetalSd" = 6, "Stucco" = 7, "Stone" = 8, "HdBoard" = 9, "Plywood" = 10,
             "BrkFace" = 11, "VinylSd" = 12, "CemntBd" = 13, "ImStucc" = 14, " Other" = 15)
startDataTrain$Exterior2nd = factor(startDataTrain$Exterior2nd, levels = names(leveling))
startDataTrain$Exterior2nd = as.integer(startDataTrain$Exterior2nd)

# MasVnrType
leveling = c("BrkCmn" = 1, "None" = 2, "BrkFace" = 3, "Stone" = 4)
startDataTrain$MasVnrType = factor(startDataTrain$MasVnrType, levels = names(leveling))
startDataTrain$MasVnrType = as.integer(startDataTrain$MasVnrType)

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

startDataTrain$OverallCond = ifelse(is.na(startDataTrain$OverallCond), 
                                  median(startDataTrain$OverallCond,
                                         na.rm = TRUE),
                                  startDataTrain$OverallCond)

startDataTrain$YearRemodAdd = ifelse(is.na(startDataTrain$YearRemodAdd), 
                                    median(startDataTrain$YearRemodAdd,
                                           na.rm = TRUE),
                                    startDataTrain$YearRemodAdd)

startDataTrain$RoofStyle = ifelse(is.na(startDataTrain$RoofStyle), 
                                     median(startDataTrain$RoofStyle,
                                            na.rm = TRUE),
                                     startDataTrain$RoofStyle)

startDataTrain$RoofMatl = ifelse(is.na(startDataTrain$RoofMatl), 
                                    median(startDataTrain$RoofMatl,
                                           na.rm = TRUE),
                                     startDataTrain$RoofMatl)

startDataTrain$Exterior1st = ifelse(is.na(startDataTrain$Exterior1st), 
                                       median(startDataTrain$Exterior1st,
                                              na.rm = TRUE),
                                    startDataTrain$Exterior1st)

startDataTrain$Exterior2nd = ifelse(is.na(startDataTrain$Exterior2nd), 
                                       median(startDataTrain$Exterior2nd,
                                              na.rm = TRUE),
                                    startDataTrain$Exterior2nd)

startDataTrain$MasVnrType = ifelse(is.na(startDataTrain$MasVnrType), 
                                      median(startDataTrain$MasVnrType,
                                             na.rm = TRUE),
                                   startDataTrain$MasVnrType)

# correlation matrix 
correlation_matrix = cor(startDataTrain)
print(correlation_matrix)

# using both datasets: ############################################################

startDataTest = dataTest[, c("MSZoning", "LotArea", "YearBuilt", "LotFrontage", 
                             "YearRemodAdd", "OverallQual", "GarageCars", "GarageArea",
                             "LandSlope", "Neighborhood", "Condition1",  
                             "BldgType", "HouseStyle", "Street", "Alley", "LotShape",
                             "LandContour", "Utilities", "LotConfig", "OverallCond",
                             "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", 
                             "MasVnrType")]

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

# OverallCond
custom_mapping <- c(`1` = 1, `2` = 4, `3` = 2, `4` = 3, `5` = 8, `6` = 5, `7` = 7, `8` = 6, `9` = 9)
New_startDataTest$OverallCond <- custom_mapping[as.character(New_startDataTest$OverallCond)]

# RoofStyle
leveling = c("Gambrel" = 1, "Gable" = 2, "Mansard" = 3, "Flat" = 4, "Hip" = 5, "Shed" = 6)
New_startDataTest$RoofStyle = factor(New_startDataTest$RoofStyle, levels = names(leveling))
New_startDataTest$RoofStyle = as.integer(New_startDataTest$RoofStyle)

# RoofMatl
leveling = c("Roll" = 1, "ClyTile" = 2, "CompShg" = 3, "Metal" = 4, "Tar&Grv" = 5, "WdShake" = 6, "Membran" = 7, "WdShngl" = 8)
New_startDataTest$RoofMatl = factor(New_startDataTest$RoofMatl, levels = names(leveling))
New_startDataTest$RoofMatl = as.integer(New_startDataTest$RoofMatl)

# Exterior1st 
leveling = c("BrkComm" = 1, "AsphShn" = 2, "CBlock" = 3, "AsbShng" = 4, "MetalSd" = 5, 
             "Wd Sdng" = 6, "WdShing" = 7, "Stucco" = 8, "HdBoard" = 9, "Plywood" = 10,
             "BrkFace" = 11, "VinylSd" = 12, "CemntBd" = 13, "Stone" = 14, "ImStucc" = 15)
New_startDataTest$Exterior1st = factor(New_startDataTest$Exterior1st, levels = names(leveling))
New_startDataTest$Exterior1st = as.integer(New_startDataTest$Exterior1st)

# Exterior2nd 
leveling = c("CBlock" = 1, "AsbShng" = 2, "Brk Cmn" = 3, "AsphShn" = 4, "Wd Sdng" = 5, 
             "MetalSd" = 6, "Stucco" = 7, "Stone" = 8, "HdBoard" = 9, "Plywood" = 10,
             "BrkFace" = 11, "VinylSd" = 12, "CemntBd" = 13, "ImStucc" = 14, " Other" = 15)
New_startDataTest$Exterior2nd = factor(New_startDataTest$Exterior2nd, levels = names(leveling))
New_startDataTest$Exterior2nd = as.integer(New_startDataTest$Exterior2nd)

# MasVnrType
leveling = c("BrkCmn" = 1, "None" = 2, "BrkFace" = 3, "Stone" = 4)
New_startDataTest$MasVnrType = factor(New_startDataTest$MasVnrType, levels = names(leveling))
New_startDataTest$MasVnrType = as.integer(New_startDataTest$MasVnrType)

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

New_startDataTest$OverallCond = ifelse(is.na(New_startDataTest$OverallCond), 
                                   median(New_startDataTest$OverallCond,
                                          na.rm = TRUE),
                                   New_startDataTest$OverallCond)

New_startDataTest$YearRemodAdd = ifelse(is.na(New_startDataTest$YearRemodAdd), 
                                     median(New_startDataTest$YearRemodAdd,
                                            na.rm = TRUE),
                                     New_startDataTest$YearRemodAdd)

New_startDataTest$RoofStyle = ifelse(is.na(New_startDataTest$RoofStyle), 
                                  median(New_startDataTest$RoofStyle,
                                         na.rm = TRUE),
                                  New_startDataTest$RoofStyle)

New_startDataTest$RoofMatl = ifelse(is.na(New_startDataTest$RoofMatl), 
                                     median(New_startDataTest$RoofMatl,
                                            na.rm = TRUE),
                                     New_startDataTest$RoofMatl)

New_startDataTest$Exterior1st = ifelse(is.na(New_startDataTest$Exterior1st), 
                                    median(New_startDataTest$Exterior1st,
                                           na.rm = TRUE),
                                    New_startDataTest$Exterior1st)

New_startDataTest$Exterior2nd = ifelse(is.na(New_startDataTest$Exterior2nd), 
                                       median(New_startDataTest$Exterior2nd,
                                              na.rm = TRUE),
                                       New_startDataTest$Exterior2nd)

New_startDataTest$MasVnrType = ifelse(is.na(New_startDataTest$MasVnrType), 
                                       median(New_startDataTest$MasVnrType,
                                              na.rm = TRUE),
                                       New_startDataTest$MasVnrType)

summary(New_startDataTest) # check to make sure there are no more NA's
summary(startDataTrain)


# split data into train and test (to test model)
set.seed(88)
split = sample.split(startDataTrain$SalePrice, SplitRatio = 0.75)
dataTrain = subset(startDataTrain, split == TRUE)
dataTest = subset(startDataTrain, split == FALSE)


# Train the Random Forest model
startModel = randomForest(SalePrice ~ ., data = dataTrain, ntree = 500)

# Predict sale prices for the test dataset
predictionStart = predict(startModel, newdata = dataTest)

mae = mean(abs(predictionStart - dataTrain$SalePrice))

# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((predictionStart - dataTrain$SalePrice)^2))

# Print the MAE and RMSE
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")

# Define a threshold for acceptable error
threshold = 10000
# Calculate accuracy as the percentage of predictions within the threshold
accuracy = mean(abs(predictionStart - dataTrain$SalePrice) < threshold)
# Print accuracy
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")
