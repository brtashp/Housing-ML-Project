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

test = dataTrain[, c("Alley", "LotShape")]
modified_test = dataTrainChar

# this does not work 
for (col_name in names(modified_test)) {
    # If the column is a factor, calculate the mean SalePrice for each unique value
    mean_sale_price = tapply(SalePrice, modified_test[[col_name]], mean)
    #ordered = order(mean_sale_price)
    rank = rank(mean_sale_price)
    # Add the results to the result data frame
    modified_test[col_name] = rank
}
print(modified_test)

summary(modified_test)

# below sorta works? 
modified_test$LotShape <- ifelse(!is.na(match(modified_test$LotShape, names(rank))), 
                              rank[match(modified_test$LotShape, names(rank))], 
                              modified_test$LotShape)

print(head(modified_test$LotShape))
summary(modified_test$LotShape)

# this works (arg arg arg)
for (col_name in names(modified_test)) {
  # If the column is a factor, calculate the mean SalePrice for each unique value
  mean_sale_price = tapply(SalePrice, modified_test[[col_name]], mean)
  #ordered = order(mean_sale_price)
  rank = rank(mean_sale_price)
  # Add the results to the result data frame
  modified_test[[col_name]] <- ifelse(!is.na(match(modified_test[[col_name]], names(rank))), 
                                   rank[match(modified_test[[col_name]], names(rank))], 
                                   modified_test[[col_name]])
  modified_test[[col_name]] = as.numeric(modified_test[[col_name]])
}

missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]
# removes the sale price column
dataTrainNum$SalePrice = NULL

dataTrainAll = cbind(dataTrainChar, dataTrainNum)
dataTrainAll$SalePrice = dataTrain$SalePrice
