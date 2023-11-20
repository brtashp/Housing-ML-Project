# submission 11/generating plots

#libraries
library(ggplot2)
library(PerformanceAnalytics)

# lets do the data exploration 
dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")
data = dataTrain$SalePrice
data <- log(data)

# Create a histogram with ggplot2
ggplot(data = data.frame(value = data), aes(x = value)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Normalized SalePrice Distribution", x = "Price", y = "Frequency") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))  

# Create a QQ plot with a linear line
qqnorm_data <- qqnorm(data)
# Extract x and y values from the qqnorm plot
qqplot_data <- data.frame(x = qqnorm_data$x, y = qqnorm_data$y)

ggplot(data = data.frame(value = data), aes(sample = value)) +
  stat_qq() +
  labs(title = "Normalized QQ Plot of SalePrice Feature", x = "Theoretical Quantiles", y = "Ordered Values") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"),  # Format y-axis labels as "K"
                     breaks = scales::pretty_breaks(n = 10)) +  # Adjust breaks for better readability
  geom_abline(intercept = mean(data), slope = sd(data), color = "blue", linetype = "solid", size = 1.5)

# correlation graphs

highcorTrain = dataTrainAll[, c(7,17,19,20,29,33,42,44,45,50,51,54,57,61,64,65,73)]
chart.Correlation(highcorTrain, histogram=TRUE, pch=19)


# heat maps
# Get some colors
# Assuming your_data is your original dataset
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = highcorTrain, col = col, symm = FALSE)