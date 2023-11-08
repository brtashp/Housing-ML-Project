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

# cleaning Train and test
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

character_columns = sapply(dataTrain, is.character)
dataTestChar = dataTest[, character_columns]

# normality testing
character_cols = character_columns

missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]
# removes the sale price column
dataTrainNum$SalePrice = NULL
numeric_col = dataTrainNum

normality_tests <- list()

for (char_col in names(character_cols)) {
  char_levels <- unique(character_cols[[char_col]])
  char_test_results <- list()
  
  for (level in char_levels) {
    subset_data <- numeric_col[character_cols[[char_col]] == level]
    
    if (is.numeric(subset_data) && length(subset_data) >= 3) {
      shapiro_result <- shapiro.test(subset_data)
      char_test_results[[as.character(level)]] <- shapiro_result$p.value > 0.05
    } else {
      # Handle non-numeric data or subsets with less than 3 data points
      char_test_results[[as.character(level)]] <- NA
    }
  }
  
  normality_tests[[char_col]] <- char_test_results
}

# part 2 of checking for normality 
# Assuming your dataset is named 'data' and you want to compare numeric columns to 'SalePrice'
target_column <- dataTrain$SalePrice

# Identify numeric columns
numeric_columns <- sapply(dataTrain, is.numeric)

# Numeric columns to check for normality
columns_to_check_numeric <- names(dataTrain)[numeric_columns]

normality_results <- data.frame(Column = character(0), PValue = numeric(0))

for (col in columns_to_check_numeric) {
  # Perform normality checks as before
  shapiro_result <- shapiro.test(dataTrain[[col]])
  
  # Append the results to the new dataframe
  normality_results <- rbind(normality_results, data.frame(Column = col, PValue = shapiro_result$p.value))
  
  # Create QQ plot and histogram if desired
  qqnorm(dataTrain[[col]])
  qqline(dataTrain[[col]])
  hist(dataTrain[[col]], breaks = 20, main = paste("Histogram of", col))
  
  cat("Shapiro-Wilk Test for", col, "p-value:", shapiro_result$p.value, "\n\n")
}

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
