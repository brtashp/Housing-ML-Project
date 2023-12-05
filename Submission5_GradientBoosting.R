# submission 5? 
# rank is based on RMSE

# load libraries 
library(caTools)
library(dplyr)
library(gbm)

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

# check for NA data
#sum(is.na(dataTestAll))
#sum(is.na(dataTrainAll))

# split data into train and test (to test model)
#set.seed(88)
#split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
#dataTrain = subset(dataTrainAll, split == TRUE)
#dataTest = subset(dataTrainAll, split == FALSE)
#dataTest2 = dataTest
#dataTest$SalePrice = NULL

# Create a gradient boosting model
gbm_model <- gbm(SalePrice ~ ., data = dataTrainAll, n.trees = 100, interaction.depth = 3)
predictions <- predict(gbm_model, newdata = dataTestAll, n.trees = 100)

IDnum = 1461:2919
MySubmission = data.frame(Id = IDnum, SalePrice = predictions)
write.csv(MySubmission, "predictionsGradientBoost.csv", row.names=FALSE)

set.seed(88)
split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
dataTrain1 = subset(dataTrainAll, split == TRUE)
dataTest1 = subset(dataTrainAll, split == FALSE)
dataTest2 = dataTest
testSalePrice = dataTest1$SalePrice
dataTest$SalePrice = NULL


gbm_model <- gbm(dataTrain1$SalePrice ~ ., data = dataTrain1,
                 n.trees = 100, interaction.depth = 3)
predictions <- predict(gbm_model, newdata = dataTestAll, n.trees = 100)



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


# Testing the predictions
#print(predictions)
#test_predictions = predictions
#actual_values <- dataTest2$SalePrice
#ssr <- sum((test_predictions - mean(actual_values))^2)
#sse <- sum((actual_values - test_predictions)^2)
#r2 <- 1 - (sse / ssr)

# Calculate metrics
#mae <- mean(abs(test_predictions - actual_values))
#mse <- mean((test_predictions - actual_values)^2)
#rmse <- sqrt(mse)

#cat(paste("MAE:", mae, "\n"))
#cat(paste("MSE:", mse, "\n"))
#cat(paste("RMSE:", rmse, "\n"))
#cat(paste("R-squared (R2):", r2, "\n"))
