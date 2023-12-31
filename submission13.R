# submission 10 
# rank is based on RMSE

# load libraries 
library(caTools)
library(randomForest)
library(dplyr)
library(ggplot2)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("FactoMineR")
library(FactoMineR)

# loading data 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")

# remove unnecessary columns 
dataTrain$Id = NULL
dataTest$Id = NULL
dataTrain$PoolQC = NULL
dataTest$PoolQC = NULL
dataTrain$MiscFeature = NULL
dataTest$MiscFeature = NULL
dataTrain$Alley = NULL
dataTest$Alley = NULL
dataTrain$Fence = NULL
dataTest$Fence = NULL
dataTrain$MSZoning = NULL
dataTest$MSZoning = NULL
dataTrain$FireplaceQu = NULL
dataTest$FireplaceQu = NULL

# this column is based on the MiscFeature, which was removed since it had over a 
# 90% NA values included. So using this feature is not going to add anything 
# since the column it is based on was also removed 
dataTrain$MiscVal = NULL
dataTest$MiscVal = NULL

# replacing NA values to true values
dataTrain$GarageQual[is.na(dataTrain$GarageQual)] <- "NG"
dataTrain$GarageCond[is.na(dataTrain$GarageCond)] <- "NG"
dataTrain$GarageFinish[is.na(dataTrain$GarageFinish)] <- "NG"
dataTrain$GarageYrBlt[is.na(dataTrain$GarageYrBlt)] <- "NG"
dataTrain$GarageType[is.na(dataTrain$GarageType)] <- "NG"
dataTrain$BsmtFinType1[is.na(dataTrain$BsmtFinType1)] <- "NB"
dataTrain$BsmtCond[is.na(dataTrain$BsmtCond)] <- "NB"
dataTrain$BsmtQual[is.na(dataTrain$BsmtQual)] <- "NB"
dataTrain$BsmtFinType2[is.na(dataTrain$BsmtFinType2)] <- "NB"
dataTrain$BsmtExposure[is.na(dataTrain$BsmtExposure)] <- "NB"

dataTest$GarageQual[is.na(dataTest$GarageQual)] <- "NG"
dataTest$GarageCond[is.na(dataTest$GarageCond)] <- "NG"
dataTest$GarageFinish[is.na(dataTest$GarageFinish)] <- "NG"
dataTest$GarageYrBlt[is.na(dataTest$GarageYrBlt)] <- "NG"
dataTest$GarageType[is.na(dataTest$GarageType)] <- "NG"
dataTest$BsmtFinType1[is.na(dataTest$BsmtFinType1)] <- "NB"
dataTest$BsmtCond[is.na(dataTest$BsmtCond)] <- "NB"
dataTest$BsmtQual[is.na(dataTest$BsmtQual)] <- "NB"
dataTest$BsmtFinType2[is.na(dataTest$BsmtFinType2)] <- "NB"
dataTest$BsmtExposure[is.na(dataTest$BsmtExposure)] <- "NB"

SalePrice1 <- log(dataTrain$SalePrice)
SalePrice <- log(dataTrain$SalePrice)
dataTrain$SalePrice = SalePrice1

#SalePrice1 = dataTrain$SalePrice
#SalePrice = dataTrain$SalePrice

# cleaning Train and test
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

character_columns = sapply(dataTest, is.character)
dataTestChar = dataTest[, character_columns]

#below works for test
for (col_name in names(dataTrainChar)) {
  mean_sale_price = tapply(SalePrice1, dataTrainChar[[col_name]], mean)
  
  # Use a different variable name for rank
  rank_values = rank(mean_sale_price)
  
  # Rename the values in dataTestChar based on rank
  dataTestChar[[col_name]] <- ifelse(!is.na(match(dataTestChar[[col_name]], names(mean_sale_price))), 
                                  rank_values[match(dataTestChar[[col_name]], names(mean_sale_price))], 
                                  dataTestChar[[col_name]])
  dataTestChar[[col_name]] = as.numeric(dataTestChar[[col_name]])
}

# below works for train 
for (col_name in names(dataTrainChar)) {
  # If the column is a factor, calculate the mean SalePrice for each unique value
  mean_sale_price = tapply(SalePrice1, dataTrainChar[[col_name]], mean)
  #ordered = order(mean_sale_price)
  rank = rank(mean_sale_price)
    
  # Add the results to the result data frame
  dataTrainChar[[col_name]] <- ifelse(!is.na(match(dataTrainChar[[col_name]], names(rank))), 
                                      rank[match(dataTrainChar[[col_name]], names(rank))], 
                                      dataTrainChar[[col_name]])
  dataTrainChar[[col_name]] = as.numeric(dataTrainChar[[col_name]])
}

# creates dataframe with only numeric/int values for train 
missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]
# removes the sale price column
dataTrainNum$SalePrice = NULL

# below for cleaning test data
# creates dataframe with only numeric/int values 
missing_columns = setdiff(names(dataTest), names(dataTestChar))
dataTestNum <- dataTest[, missing_columns]

# combines everything into one dataframe for train data 
dataTrainAll = cbind(dataTrainChar, dataTrainNum)
dataTrainAll$SalePrice = dataTrain$SalePrice

# corrects NAs for train data
dataTrainAll[] <- lapply(dataTrainAll, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# combines everything into one dataframe 
dataTestAll = cbind(dataTestChar, dataTestNum)

# changes NA to mean 
dataTestAll <- dataTestAll %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


# clustering methods
# Perform k-means clustering with k=3 (you can choose another value)
k <- 3
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(dataTrainAll, centers = k)

# View the cluster assignments
cluster_assignments <- kmeans_result$cluster
print(cluster_assignments)

# View the cluster centers
cluster_centers <- kmeans_result$centers
print(cluster_centers)

# testing the accuracy (need to use the train data to get the accuracy) ###
startModel = randomForest(SalePrice ~ ., data = dataTrainAll, ntree = 1000)

# Predict sale prices for the test dataset
predictions1 = predict(startModel, newdata = dataTestAll)
predictions = exp(predictions1)

IDnum = 1461:2919
MySubmission = data.frame(Id = IDnum, SalePrice = predictions)
write.csv(MySubmission, "predictionsRandomForest.csv", row.names=FALSE)

#mae = mean(abs(predictions - dataTrain$SalePrice))
# Calculate the Root Mean Squared Error (RMSE)
#rmse = sqrt(mean((predictions - dataTrain$SalePrice)^2))
# Print the MAE and RMSE
#cat("Mean Absolute Error (MAE): ", mae, "\n")
#cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
# Define a threshold for acceptable error
#threshold = 10000
# Calculate accuracy as the percentage of predictions within the threshold
#accuracy = mean(abs(predictionst - dataTrain$SalePrice) < threshold)
# Print accuracy
#cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")