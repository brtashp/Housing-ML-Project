# Stat data intelligence project - predict the final price of the home 
# 

# load data 

data = read.csv('train.csv')
str(data)
summary(data)

# data cleaning 
# need to sort char data into int values (categorial to numerical values)
# first by assigning values to char condition ratings 

library(ggplot2)

hist(data$SalePrice)
