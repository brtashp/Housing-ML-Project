# submission 5.1 
# testing other methods of predicitive modeling

# load libraries 
library(caTools)
library(class)
library(dplyr)

# loading data 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")
# remove unnecessary columns 
dataTrain$Id = NULL
dataTest$Id = NULL
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

# combines everything into one dataframe for train data 
dataTrainAll = cbind(dataTrainChar, dataTrainNum)
dataTrainAll$SalePrice = dataTrain$SalePrice

# corrects NAs for train data
dataTrainAll[] <- lapply(dataTrainAll, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})
#below might also work? 
#dataTrainAll[] <- lapply(dataTrainAll, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# below for cleaning test data
# creates dataframe with only numeric/int values 
missing_columns = setdiff(names(dataTest), names(dataTestChar))
dataTestNum <- dataTest[, missing_columns]

# combines everything into one dataframe 
dataTestAll = cbind(dataTestChar, dataTestNum)

# changes NA to mean 
dataTestAll <- dataTestAll %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

summary(dataTestAll)

# correlation matrix 
correlation = cor(dataTrainAll)
print(correlation)

# split data into train and test (to test model)
set.seed(88)
split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
dataTrain1 = subset(dataTrainAll, split == TRUE)
dataTest1 = subset(dataTrainAll, split == FALSE)

# testing the accuracy (need to use the train data to get the accuracy) ###
# Fit a K-NN model

k_value = 6
predictions <- knn(train = dataTrain1, test = dataTest1, 
                   cl = dataTrain1$SalePrice, k = k_value)

knn_model <- knn(train = dataTrain1, test = dataTest1, cl = dataTrain1$SalePrice, k = k_value)
actaulSalePrice = dataTest1$SalePrice
predictionTest = predictions

# mae results 
mae = mean(abs(actaulSalePrice - predictionTest))
# mse results
# Assuming y_true and y_pred are your actual and predicted values
mse = mean((actaulSalePrice - predictionTest)^2)
# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((actaulSalePrice - predictionTest)^2))
# mape results
mape = mean(abs((actaulSalePrice - predictionTest) / actaulSalePrice)) * 100
# Define a threshold for acceptable error
threshold = 10000
# Calculate accuracy as the percentage of predictions within the threshold
accuracy = mean(abs(actaulSalePrice - predictionTest) < threshold)

# Print results
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")


# Predicted values
y_pred = knn(train = dataTrain1, test = dataTest1, cl = dataTrain1$SalePrice, k = k_value)

# Mean Absolute Error (MAE)
mae <- mean(abs(y_true - y_pred))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Mean Squared Error (MSE)
mse <- mean((y_true - y_pred)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# R-squared
y_mean <- mean(y_true)
rsquared <- 1 - sum((y_true - y_pred)^2) / sum((y_true - y_mean)^2)
cat("R-squared:", rsquared, "\n")
