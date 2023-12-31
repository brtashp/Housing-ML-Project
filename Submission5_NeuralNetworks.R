# submission 5
# rank is based on RMSE

# load libraries 
library(caTools)
library(dplyr)
library(keras)

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

#summary(dataTestAll)

# correlation matrix 
#correlation = cor(dataTrainAll)
#print(correlation)

# split data into train and test (to test model)
set.seed(88)
split = sample.split(dataTrainAll$SalePrice, SplitRatio = 0.75)
dataTrain = subset(dataTrainAll, split == TRUE)
dataTest = subset(dataTrainAll, split == FALSE)

# Exclude the "SalePrice" column by indexing
scaled_train_data <- dataTrain[, -which(names(dataTrain) == "SalePrice")]
scaled_test_data <- dataTest[, -which(names(dataTest) == "SalePrice")]

# Define the neural network architecture
model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(scaled_train_data)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")  # Linear activation for regression

# Compile the model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(lr = 0.001)
)

# Extract the target variable (SalePrice) from the training data
train_labels <- dataTrain$SalePrice

# Fit the model
history <- model %>% fit(
  x = scaled_train_data,
  y = train_labels,
  epochs = 100,  # You can adjust the number of epochs
  batch_size = 32,
  validation_split = 0.2  # Optionally specify a validation set
)

# Extract the target variable (SalePrice) from the testing data
test_labels <- dataTest$SalePrice

# Make predictions on the test data
predictions <- model %>% predict(scaled_test_data)

# Calculate metrics (e.g., Mean Absolute Error)
mae <- mean(abs(predictions - test_labels))
cat("Mean Absolute Error:", mae, "\n")
