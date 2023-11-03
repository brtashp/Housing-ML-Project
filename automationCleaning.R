# automating data cleaning 

dataTrain = read.csv("train.csv")

column_data_type = class(dataTrain[, 4])
print(column_data_type)

character_columns = sapply(dataTrain, is.character)
print(character_columns)
# puts all the character data from original dataset into new dataframe 
dataTrainChar = dataTrain[, character_columns]
# adds the sale price column to the char dataset, dont use yet 
#dataTrainChar$SalePrice = dataTrain$SalePrice
SalePrice = dataTrain$SalePrice

# adds the sale price column to a new dataframe with only the character values 
SalePriceChar = cbind(SalePrice, dataTrainChar)

result = tapply(SalePrice, SalePriceChar$MSZoning, mean)
result2 = tapply(SalePrice, SalePriceChar$Street, mean) 

result3 = cbind(result, result2)
print(result)

for (col_name in names(SalePriceChar)) {
  if (is.factor(dataTrainChar[[col_name]])) {
    # If the column is a factor, calculate the mean SalePrice for each unique value
    mean_sale_price <- tapply(SalePrice, dataTrainChar[[col_name+1]], mean)
    
    # Convert the result into a data frame
    cleanedNumChar = data.frame(Value = levels(dataTrainChar[[col_name]]), MeanSalePrice = mean_sale_price)
    
    # Add the results to the result data frame
    result = cbind(result, cleanedNumChar)
  }
}
print(result)
n = length(result)

result = aggregate(cbind(Value1, Value2) ~ Group, data, sum)

result = tapply(dataTrain$SalePrice, dataTrainChar$MSZoning, mean)
print(result)

result = tapply(dataTrain$SalePrice, dataTrainChar$Street, mean)

for (col in dataTrain)
  

result_list = list()

for (col in dataTrain) {
    result = aggregate(dataTrain$SalePrice, by = list(dataTrain[[col]]), FUN = mean)
    names(result) <- c(col, "MeanSalePrice")
    result_list[[col]] <- result
}

for (col in dataTrainChar) {
  result = 
  resultdf = c(resultdf, dataTrainChar[col] + result[col])
  combined_vector = c(combined_vector, original_vector[i] + new_data[i])
}


# example of what we need to do first: 
leveling = c("C" = 1, "RM" = 2, "RH" = 3, "RL" = 4, "FV" = 5)
startDataTrain$MSZoning = factor(startDataTrain$MSZoning, levels = names(leveling))
startDataTrain$MSZoning = as.integer(startDataTrain$MSZoning)
  