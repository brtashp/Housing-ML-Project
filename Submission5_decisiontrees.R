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
dataTrain = subset(dataTrainAll, split == TRUE)
dataTest = subset(dataTrainAll, split == FALSE)

# testing the accuracy (need to use the train data to get the accuracy) ###
target_variable = dataTrain$SalePrice
predictor_variables = dataTrain[, !names(dataTrain) %in% "SalePrice"]

# Fit a decision tree model on the training data (dataTrain)
tree_model = rpart(target_variable ~ ., data = dataTrain)
plot(tree_model)

# Create a new data frame for prediction using the testing dataset (datatest)
new_data = dataTest[, names(dataTest) %in% names(predictor_variables)]

# Make predictions on the testing data
predictions <- predict(tree_model, new_data, type = "class")

# Assuming you have defined your target variable and predictor variables from the training dataset
target_variable <- dataTrain$SalePrice
predictor_variables <- dataTrain[, !names(dataTrain) %in% "SalePrice"]

# Fit a decision tree model on the training data (dataTrain)
tree_model <- rpart(target_variable ~ ., data = dataTrain)
plot(tree_model)

# Create a new data frame for prediction using the testing dataset (datatest)
new_data <- dataTest[, names(dataTest) %in% names(predictor_variables)]

# Make predictions on the testing data
predictions <- predict(tree_model, new_data, type = "class")