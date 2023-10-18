# Stat data intelligence project - predict the final price of the home 

# libraries: 
library(ggplot2)

# load data 
data = read.csv('train.csv')
str(data)
summary(data)

# data cleaning 
# need to sort char data into int values (categorical to numerical values)
# first by assigning values to char condition ratings 
# also need to delete unnecessary variables, like:
# ID,  



hist(data$SalePrice)
