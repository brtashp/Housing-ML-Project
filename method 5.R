# method 5
# use of PCA, use of linear regression, and further improvements
# using less features, only highly correlated 
# try other models? 

# load libraries 
library(caTools)
library(randomForest)
library(dplyr)
library(ggplot2)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(stats)
library(caret)

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
# highest correlated values
pca_model <- prcomp(dataTrainAll, scale. = TRUE)
# Transform train_data using the PCA model
#train_data_pca <- predict(pca_model, dataTrainAll)
# Transform test_data using the same PCA model
#test_data_pca <- predict(pca_model, dataTestAll)

# dont forget data reduction (see below)
# stats
stdev = pca_model$sdev
var = stdev^2
prop_varex = var/sum(var)

# automated stats 
cumulative_explained_variance = cumsum(pca_model$sdev^2 / sum(pca_model$sdev^2))
n_components <- which(cumulative_explained_variance >= 0.95)[1]
pca_model2 <- prcomp(dataTrainAll, scale. = TRUE, rank = n_components)
#plot(cumsum(prop_varex), type="b")
train_data_pca <- predict(pca_model2, dataTrainAll)
test_data_pca <- predict(pca_model2, dataTestAll)

#biplot(pca_model, scale = 0)

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

# results of the model #########################################################

# split train data into own train and test to test model 
# split data into train and test (to test model)

# Set seed for reproducibility
set.seed(88)
dataTrainAll$SalePrice = SalePrice
num_samples <- nrow(dataTrainAll)
splitIndex <- sample(1:num_samples, size = 0.75 * num_samples)
# Split the data based on the index
resultsdataTrain <- dataTrainAll[splitIndex, ]
resultsdataTrain <- as.data.frame(resultsdataTrain)
resultsdataTest <- dataTrainAll[-splitIndex, ]
resultsdataTest <- as.data.frame(resultsdataTest)
actaulSalePrice = resultsdataTest$SalePrice
actualTrainSalePrice = resultsdataTrain$SalePrice
resultsdataTest$SalePrice = NULL
resultsdataTrain$SalePrice = NULL

# rerun model using new test and train data
pca_model <- prcomp(resultsdataTrain, scale. = TRUE)
cumulative_explained_variance = cumsum(pca_model$sdev^2 / sum(pca_model$sdev^2))
n_components = which(cumulative_explained_variance >= 0.95)[1]
pca_model3 = prcomp(resultsdataTrain, scale. = TRUE, rank = n_components)

resultsdataTrain$SalePrice = NULL
train_data_pca <- predict(pca_model3, resultsdataTrain)
test_data_pca <- predict(pca_model3, resultsdataTest)

startModel = lm(actualTrainSalePrice ~ ., data = resultsdataTrain)
predictions1 = predict(startModel, newdata = resultsdataTest)
predictionTest = exp(predictions1)
actaulSalePrice = exp(actaulSalePrice)


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
# and y_true and y_pred are your actual and predicted values
adj_r_squared = 1 - (1 - summary(startModel)$r.squared) * (length(actaulSalePrice) - 1) / 
  (length(actaulSalePrice) - length(startModel$coefficients) - 1)

eigenvalues <- pca_model3$sdev^2
plot(1:length(eigenvalues), eigenvalues, type = "b", pch = 19, 
     main = "Scree Plot", xlab = "Principal Component", ylab = "Eigenvalue")
grid()

axis(1, at = 1:length(eigenvalues), labels = paste("PC", 1:length(eigenvalues)))

# Print results
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Mean Squared Error (MSE): ", mse, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE): ", mape, "\n")
cat("Accuracy within $", threshold, ": ", accuracy * 100, "%\n")
cat("Adjusted R^2: ", adj_r_squared, "\n")

# finding most significant features (p < 0.05)
# Extract coefficients and their significance levels
coefficients_summary = summary(startModel)$coefficients
significant_coefficients = coefficients_summary[coefficients_summary[, "Pr(>|t|)"] < 0.05, c("Estimate", "Pr(>|t|)")]
print(significant_coefficients)