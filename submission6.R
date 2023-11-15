# submission 6 
# need to fix the normality of the data in the data cleaning portion 
# use mean for normally distributed data and median for not normally distributed 

# load libraries 
library(caTools)
library(dplyr)

# loading data 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")

# remove unnecessary columns 
dataTrain$Id = NULL
dataTest$Id = NULL
SalePrice1 = dataTrain$SalePrice

# remove unnecessary columns
dataTrain$Id = NULL
dataTest$Id = NULL

# identify data we dont need before cleaning based on percentage of missing data
# Calculate the total number of missing values for each column
total <- colSums(is.na(dataTrain))
# Calculate the percentage of missing values for each column
percent <- (colSums(is.na(dataTrain)) / nrow(dataTrain)) * 100
# Create a data frame to store the total and percentage of missing values
missing_data <- data.frame(Total = total, Percent = percent)
# Sort the data frame by the percentage of missing values in descending order
missing_data <- missing_data[order(-missing_data$Percent), ]
# Display the first 20 rows of the sorted data frame
head(missing_data, 20)
# remove more data we dont need
dataTrain$PoolQC = NULL
dataTest$PoolQC = NULL
dataTrain$MiscFeature = NULL
dataTest$MiscFeature = NULL
dataTrain$Alley = NULL
dataTest$Alley = NULL
dataTrain$Fence = NULL
dataTest$Fence = NULL


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
