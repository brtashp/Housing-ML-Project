# submission 5? 
# rank is based on RMSE

# load libraries 
library(caTools)
library(randomForest)
library(dplyr)
library(ggplot2)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

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

SalePrice = dataTrain$SalePrice

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

#summary(dataTestAll)

# correlation matrix 
#correlation = cor(dataTrainAll)
#print(correlation)

# split data into train and test (to test model)
#set.seed(88)
#split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
#dataTrain = subset(dataTrainAll, split == TRUE)
#dataTrain = subset(dataTrainAll, split == FALSE)

highcorTrain = dataTrainAll[, c(7,17,19,20,29,33,42,44,45,50,51,54,57,61,64,65,75)]
highcorTest = dataTestAll[, c(7,17,19,20,29,33,42,44,45,50,51,54,57,61,64,65)]

# testing the accuracy (need to use the train data to get the accuracy) ###
startModel = randomForest(SalePrice ~ ., data = highcorTrain, ntree = 500)

# Predict sale prices for the test dataset
predictions = predict(startModel, newdata = highcorTest)

IDnum = 1461:2919
MySubmission = data.frame(Id = IDnum, SalePrice = predictions)
write.csv(MySubmission, "predictionsRandomForest.csv", row.names=FALSE)

# below for creating correlation matrixes 
my_data = dataTrainAll[, c(1,2,3,4,5,6,7,8,9,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(10,11,12,13,14,15,16,17,18,19,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(20,21,22,23,24,25,26,27,28,29,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(30,31,32,33,34,35,36,37,38,39,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(40,41,42,43,44,45,46,47,48,49,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(50,51,52,53,54,55,56,57,58,59,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(60,61,62,63,64,65,66,67,68,69,75)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data = dataTrainAll[, c(70,71,72,73,74,75)]

chart.Correlation(highcorTrain, histogram=TRUE, pch=19)

#write.csv(correlation, "correlationdata.CSV", row.names=FALSE)

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