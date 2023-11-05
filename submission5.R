# submission 5? 

# load libraries 
library(caTools)

# loading data 
data = read.csv("train.csv")

# split data into train and test 
set.seed(88)
split = sample.split(data$SalePrice, SplitRatio = 0.75)
dataTrain = subset(quality, split == TRUE)
dataTest = subset(quality, split == FALSE)

# cleaning 
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

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


# testing the accuracy (need to use the train data to get the accuracy)
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