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

for (column in names(numerical_data)) {
  p <- ggplot(data, aes(x = data[[column]])) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black") +
    labs(x = "X-axis Label", y = "Frequency", title = paste("Histogram for", column)) +
    theme_minimal()
  
  # Use print() to display each plot individually
  print(p)
  
  # Save the plot as an image
  ggsave(paste(column, "_histogram.png"), plot = p, width = 6, height = 4)
}

