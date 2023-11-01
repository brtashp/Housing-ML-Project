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


result = tapply(dataTrainChar$SalePrice, dataTrainChar$MSZoning, mean)
print(result)

result_list = list()

for (col in dataTrainChar) {
  if (is.character(dataTrain[[col]])) {
    result <- aggregate(dataTrain$SalePrice, by = list(dataTrain[[col]]), FUN = mean)
    names(result) <- c(col, "MeanSalePrice")
    result_list[[col]] <- result
  }
}