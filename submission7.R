# submission 7?
# were only on the 4th submission i think?

library(caTools)
library(dplyr)

# functions created
# Function to remove outliers based on z-scores within each level of each column
remove_outliers <- function(df, variable, factor_variable, threshold = 3) {
  df %>%
    group_by({{ factor_variable }}) %>%
    mutate(z_score = scale({{ variable }})) %>%
    filter(abs(z_score) <= threshold) %>%
    ungroup() %>%
    select(-z_score)
}

dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")

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

SalePrice1 = dataTrain$SalePrice

# creating a character only column
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

# turn character values in numeric 
dataTrainCharNum = dataTrainChar
for (col_name in names(dataTrainChar)) {
  factor_vector <- factor(dataTrainChar[[col_name]])
  dataTrainCharNum[[col_name]] <- as.numeric(factor_vector)
}


# removing outliers 
# adds the saleprice column to the dataframe
dataTrainCharNum = data.frame(dataTrainCharNum, SalePrice = SalePrice1)

dataTrainCharClean = data.frame()


cleaned = list()
cleaned = remove_outliers(dataTrainCharNum, SalePrice, dataTrainCharNum$MSZoning)
dataTrainCharNum <- dataTrainCharNum[, -which(names(dataTrainCharNum) == "MSZoning")]
for (col_name in names(dataTrainCharNum)) {
  cleaned = remove_outliers(cleaned, SalePrice, cleaned[[col_name]])
}

cleaned = remove_outliers(dataTrainCharNum, SalePrice, dataTrainCharNum$MSZoning)
cleaned = remove_outliers(cleaned, SalePrice, cleaned$Street)
cleaned = remove_outliers(cleaned, SalePrice, cleaned$LotShape)




dataTrainCharNum = data.frame(dataTrainCharNum, SalePrice = SalePrice1)
# below works now, need to have both SalePrice and factor variable in the same 
# dataset, or add the saleprice back, see above
# idea: maybe add the numeric columns back and then remove the outliers?
dataTrainCharClean = remove_outliers(dataTrainCharNum, SalePrice, MSZoning)


boxplot(dataTrain$SalePrice ~ dataTrain$MSZoning)
boxplot(cleaned_data$SalePrice ~ cleaned_data$MSZoning)



# for loop example 
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