# submission 7?
# were only on the 4th submission i think?

library(caTools)
library(dplyr)

dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")

character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]

# Convert it to a factor
factor_vector <- factor(dataTrainChar$MSZoning)

# Convert the factor levels to numeric values
numeric_vector <- as.numeric(factor_vector)

# Numeric values assigned to character values
print(numeric_vector)




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