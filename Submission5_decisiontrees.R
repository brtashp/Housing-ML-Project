# submission 5.2
# testing other methods of predicitive modeling

# load libraries 
library(caTools)
library(dplyr)
library(rpart)

# loading data 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")
# remove unnecessary columns 
dataTrain$Id = NULL
dataTest$Id = NULL
SalePrice1 = dataTrain$SalePrice

# cleaning Train and test
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

character_columns = sapply(dataTrain, is.character)
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

#summary(dataTestAll)

# correlation matrix 
#correlation = cor(dataTrainAll)
#print(correlation)

# split data into train and test (to test model)
set.seed(88)
split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
dataTrain1 = subset(dataTrainAll, split == TRUE)
dataTest1 = subset(dataTrainAll, split == FALSE)
dataTest2 = dataTest
trainSalePrice = dataTrain1$SalePrice
testSalePrice = dataTest1$SalePrice
dataTest$SalePrice = NULL

#dataTrainAll <- dataTrainAll[-2, ]

# Extract the target variable and predictor variables from dataTrainAll
target_variable <- dataTrain1$SalePrice
predictor_variables <- dataTrain1[, !names(dataTrain1) %in% "SalePrice"]

# Fit a decision tree model on the training data (dataTrain)
dataTrain1 <- dataTrain1[, !names(dataTrain1) %in% "SalePrice"]

SalePrice1 = dataTrain1$SalePrice

tree_model <- rpart(trainSalePrice ~ ., data = dataTrain1)

# using dyplr lib
tree_model <- rpart(target_variable ~ ., data = dataTrain1[, !names(dataTrain1) %in% "SalePrice"])
plot(tree_model)

# Make predictions on the testing data
predictions <- predict(tree_model, dataTest1, type = "vector")

# testing the accuracy 
actual_values = dataTest2$SalePrice

actaulSalePrice = testSalePrice
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


install.packages("caret")
library(caret)

data <- data.frame(obs = testSalePrice, pred = predictions)

# Calculate regression metrics
metrics <- defaultSummary(data)
print(metrics)

# Calculate the Mean Squared Error (MSE)
#mse <- mean((predictions - actual_values)^2)

# Calculate R-squared (R^2)
#ss_res <- sum((actual_values - predictions)^2)
#ss_tot <- sum((actual_values - mean(actual_values))^2)
#rsquared <- 1 - (ss_res / ss_tot)

# Print the MSE and R-squared
#cat("Mean Squared Error (MSE):", mse, "\n")
#cat("R-squared (R^2):", rsquared, "\n")

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