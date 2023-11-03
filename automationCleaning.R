# automating data cleaning 

dataTrain = read.csv("train.csv")

character_columns = sapply(dataTrain, is.character)
print(character_columns)
# puts all the character data from original dataset into new dataframe 
dataTrainChar = dataTrain[, character_columns]
# adds the sale price column to the char dataset, dont use yet 
#dataTrainChar$SalePrice = dataTrain$SalePrice
SalePrice = dataTrain$SalePrice

# adds the sale price column to a new dataframe with only the character values 
SalePriceChar = cbind(SalePrice, dataTrainChar)

test = dataTrain[, c("MSZoning", "Street", "Alley")]
modified_test = test

for (col_name in names(modified_test)) {
    # If the column is a factor, calculate the mean SalePrice for each unique value
    mean_sale_price = tapply(SalePrice, modified_test[[col_name]], mean)
    ordered = order(mean_sale_price)
    rank = rank(ordered)
    # Add the results to the result data frame
    modified_test[col_name] = rank
}
print(modified_test)
