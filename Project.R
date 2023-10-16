# Stat data intelligence project - predict the final price of the home 

# libraries: 
library(ggplot2)

# notes: 
# need to remove id since its not really necessary

# load data 
data = read.csv('train.csv')
str(data)
summary(data)

# data cleaning 
# need to sort char data into int values (categorial to numerical values)
# first by assigning values to char condition ratings 

library(ggplot2)

hist(data$SalePrice)

numerical_data <- data[sapply(data, is.numeric)]

for (column in names(numerical_data)) {
  if (!is.null(data[[column]]) && !anyNA(data[[column]])) {
    
    # Calculate an appropriate binwidth using Scott's Rule
    binwidth <- diff(range(data[[column]])) / (3.5 * IQR(data[[column]]) / length(data[[column]])^(1/3))
    
    # Check if the binwidth is non-finite or very small
    if (!is.finite(binwidth) || binwidth < 1e-6) {
      # Set a reasonable binwidth for very small ranges
      binwidth <- 1
    }
    
    p <- ggplot(data, aes(x = .data[[column]])) +
      geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
      labs(x = "X-axis Label", y = "Frequency", title = paste("Histogram for", column)) +
      theme_minimal()
    
    # Use print() to display each plot individually
    print(p)
    
    # Save the plot as an image
    ggsave(paste(column, "_histogram.png"), plot = p, width = 6, height = 4)
  }
}

