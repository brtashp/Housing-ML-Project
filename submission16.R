# submission 14, 0.13712 
# rank is based on RMSE
# trying to normalize the highest correlated values

# load libraries 
library(caTools)
library(randomForest)
library(dplyr)
library(ggplot2)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(stats)

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

#SalePrice = dataTrain$SalePrice

# log transforming SalePrice since it is not normalized 
SalePrice1 <- log(dataTrain$SalePrice)
SalePrice <- log(dataTrain$SalePrice)
dataTrain$SalePrice = SalePrice1

# cleaning Train and test
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

character_columns = sapply(dataTrain, is.character)
dataTestChar = dataTest[, character_columns]

#below works for test
for (col_name in names(dataTrainChar)) {
  mean_sale_price = tapply(SalePrice, dataTrainChar[[col_name]], mean)
  
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
  mean_sale_price = tapply(SalePrice, dataTrainChar[[col_name]], mean)
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
#dataTrainAll$SalePrice = dataTrain$SalePrice

# corrects NAs for train data
dataTrainAll[] <- lapply(dataTrainAll, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# combines everything into one dataframe 
dataTestAll = cbind(dataTestChar, dataTestNum)

# changes NA to mean 
dataTestAll <- dataTestAll %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# mutate some of the data based on histogram shapes
# left/right skewed data
#dataTrainAll$TotalBsmtSF = log(dataTrain$TotalBsmtSF) # causes infinite values
#dataTestAll$TotalBsmtSF = log(dataTest$TotalBsmtSF)
dataTrainAll$X1stFlrSF = log(dataTrain$X1stFlrSF)
dataTestAll$X1stFlrSF = log(dataTest$X1stFlrSF)
dataTrainAll$GrLivArea = log(dataTrain$GrLivArea)
dataTestAll$GrLivArea = log(dataTest$GrLivArea)

# using PCA methods 
pca_model <- prcomp(dataTrainAll, scale. = TRUE)
# Transform train_data using the PCA model
train_data_pca <- predict(pca_model, dataTrainAll)
# Transform test_data using the same PCA model
test_data_pca <- predict(pca_model, dataTestAll)
dataTrainAll$SalePrice = dataTrain$SalePrice

# multimodal data
# use factoring to separate into separate columns
#dataTrainAll$ExterQual = as.factor()

#any(is.infinite(dataTrainAll$TotalBsmtSF))

# Check for NaN values
any(is.nan(dataTrainAll))

# testing the accuracy (need to use the train data to get the accuracy) ###
dataframe_datatrain <- as.data.frame(train_data_pca)
dataframe_datatest <- as.data.frame(test_data_pca)
startModel <- lm(SalePrice ~ ., data = dataframe_datatrain)

# Predict sale prices for the test dataset
#predictions = predict(startModel, newdata = dataTestAll)
predictions1 = predict(startModel, newdata = dataframe_datatest)
predictions = exp(predictions1)

IDnum = 1461:2919
MySubmission = data.frame(Id = IDnum, SalePrice = predictions)
write.csv(MySubmission, "predictionslinearRegression.csv", row.names=FALSE)

chart.Correlation(highcorTrain, histogram=TRUE, pch=19)

# correlation matrix 
#correlation = cor(dataTrainAll)
#print(correlation)

# split data into train and test (to test model)
#set.seed(88)
#split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
#dataTrain = subset(dataTrainAll, split == TRUE)
#dataTrain = subset(dataTrainAll, split == FALSE)

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