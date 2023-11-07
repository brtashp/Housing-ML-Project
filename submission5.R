# submission 5? 
# rank is based on RMSE

# load libraries 
library(caTools)
library(randomForest)

# loading data 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")
# remove unnecessary columns 
dataTrain$Id = NULL
dataTest$Id = NULL
SalePrice = dataTrain$SalePrice

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

# creates dataframe with only numeric/int values 
missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]
# removes the sale price column
dataTrainNum$SalePrice = NULL

# combines everything into one dataframe 
dataTrainAll = cbind(dataTrainChar, dataTrainNum)
dataTrainAll$SalePrice = dataTrain$SalePrice

# corrects NAs
dataTrainAll[] <- lapply(dataTrainAll, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# cleaning for test values ###################################################
character_columns = sapply(dataTrain, is.character)
dataTestChar = dataTest[, character_columns]

for (col_name in names(dataTrainChar)) {
  # If the column is a factor, calculate the mean SalePrice for each unique value
  mean_sale_price = tapply(SalePrice, dataTrainChar[[col_name]], mean)
  
  # Use a different variable name for rank
  rank_values = rank(mean_sale_price)
  
  # Rename the values in dataTestChar based on rank
  dataTestChar[[col_name]] <- ifelse(!is.na(match(dataTestChar[[col_name]], names(mean_sale_price))), 
                                     rank_values[match(dataTestChar[[col_name]], names(mean_sale_price))], 
                                     dataTestChar[[col_name]])
  dataTestChar[[col_name]] = as.integer(dataTestChar[[col_name]])
}


for (col_name in names(dataTrainChar)) {
  # If the column is a factor, calculate the mean SalePrice for each unique value
  mean_sale_price = tapply(SalePrice, dataTrainChar[[col_name]], mean)
  
  # Use a different variable name for rank
  rank_values = rank(mean_sale_price)
  
  # Rename the values in dataTestChar based on rank
  dataTestChar[[col_name]] <- ifelse(!is.na(match(dataTestChar[[col_name]], names(mean_sale_price))), 
                                     rank_values[match(dataTestChar[[col_name]], names(mean_sale_price))], 
                                     dataTestChar[[col_name]])
  dataTestChar[[col_name]] = as.numeric(dataTestChar[[col_name]])
}
  
mean_sale_price = tapply(SalePrice, dataTrainChar$MSZoning, mean)

# Use a different variable name for rank
rank_values = rank(mean_sale_price)

# Rename the values in dataTestChar based on rank
dataTestChar$MSZoning <- ifelse(!is.na(match(dataTestChar$MSZoning, names(mean_sale_price))), 
                                   rank_values[match(dataTestChar$MSZoning, names(mean_sale_price))], 
                                   dataTestChar$MSZoning)
dataTestChar$MSZoning = as.numeric(dataTestChar$MSZoning)

# works after only running the dataTestChar functions first, not with lots of data in there

# creates dataframe with only numeric/int values 
missing_columns = setdiff(names(dataTest), names(dataTestChar))
dataTestNum <- dataTest[, missing_columns]

# combines everything into one dataframe 
dataTestAll = cbind(dataTestChar, dataTestNum)

# corrects NAs
dataTestAll <- lapply(dataTestAll, function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

summary(dataTestAll)

# correlation matrix 
#correlation = cor(dataTrainAll)
#print(correlation)

# split data into train and test (to test model)
#set.seed(88)
#split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
#dataTrain = subset(dataTrainAll, split == TRUE)
#dataTest = subset(dataTrainAll, split == FALSE)



# testing the accuracy (need to use the train data to get the accuracy)
startModel = randomForest(SalePrice ~ ., data = dataTrainAll, ntree = 500)

# Predict sale prices for the test dataset
predictionStart = predict(startModel, newdata = dataTestAll)

mae = mean(abs(predictionStart - dataTrainAll$SalePrice))

# Calculate the Root Mean Squared Error (RMSE)
rmse = sqrt(mean((predictionStart - dataTrainAll$SalePrice)^2))

# Print the MAE and RMSE
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")

# Define a threshold for acceptable error
threshold = 10000
# Calculate accuracy as the percentage of predictions within the threshold
accuracy = mean(abs(predictionStart - dataTrain$SalePrice) < threshold)
# Print accuracy
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")
