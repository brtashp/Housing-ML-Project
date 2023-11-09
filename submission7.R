# submission 7?
# were only on the 4th submission i think?

library(caTools)
library(dplyr)

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

# creating a character only column
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

dataTrainCharNum = dataTrainChar
for (col_name in names(dataTrainChar)) {
  # turn character values in numeric 
  factor_vector <- factor(dataTrainChar[[col_name]])
  dataTrainCharNum[[col_name]] <- as.numeric(factor_vector)
  # removing outliers 
  
}
# i dont like this method below 
SalePrice1 = dataTrain$SalePrice
average_saleprice_by_column <- tapply(SalePrice1, dataTrainCharNum[, "MSZoning"], mean)
threshold <- 2 * sd(SalePrice1)
for (value in unique(dataTrainCharNum$MSZoning)) {
  outlier_rows <- abs(SalePrice1 - average_saleprice_by_column[value]) > threshold
  dataTrainCharNum <- dataTrainCharNum[!(dataTrainCharNum$MSZoning == value & outlier_rows), ]
}

column_of_interest <- "MSZoning"
# Calculate the quartiles and IQR for the "SalePrice" variable
Q1 <- quantile(SalePrice1, 0.25)
Q3 <- quantile(SalePrice1, 0.75)
IQR <- Q3 - Q1
# Set lower and upper bounds for identifying outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove rows where the "SalePrice" for each "color" value falls outside the bounds
for (color_value in unique(dataTrainCharNum$MSZoning)) {
  outlier_rows <- (dataTrainCharNum$MSZoning == color_value) & (dataTrainCharNum$SalePrice < lower_bound | dataTrainCharNum$SalePrice > upper_bound)
  dataTrainCharNum <- dataTrainCharNum[!outlier_rows, ]
}
length(dataTrainCharNum$MSZoning)











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