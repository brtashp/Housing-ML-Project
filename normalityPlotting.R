# identifying normality within data code

library(ggplot2)
#install.packages("ggstatsplot")
# Load the package
library(ggstatsplot)

dataTrain = read.csv("train.csv")
dataTest = read.csv("test.csv")

# removes outliers (for each row) for character (so far doesnt work for character values)
boxplot(dataTrain$SalePrice ~ dataTrain$MSZoning)
outliers = boxplot(dataTrain$SalePrice ~ dataTrain$MSZoning, plot=FALSE)$out
dataTrainOut = dataTrain
dataTrain = dataTrain[-which(dataTrain$MSZoning %in% outliers),]
boxplot(dataTrain$SalePrice ~ dataTrain$MSZoning)

# for numeric values 
boxplot(dataTrain$SalePrice ~ dataTrain$LotFrontage)
# na.rm = TRUE is used to ignore the NA values 
outliers = boxplot(dataTrain$LotFrontage, plot=FALSE, na.rm = TRUE)$out
#dataTrainOut = dataTrain
dataTrain1 = dataTrain[-which(dataTrain$LotFrontage %in% outliers),]


# for loop for removing outliers in numeric data
character_columns = sapply(dataTrain, is.character)
dataTrainChar = dataTrain[, character_columns]
missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]

summary(dataTrain)
sum(is.na(dataTrain$LotFrontage))

# below works now 
Q <- quantile(dataTrain$LotFrontage, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(dataTrain$LotFrontage, na.rm = TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(dataTrain$LotFrontage, dataTrain$LotFrontage > (Q[1] - 1.5*iqr) & dataTrain$LotFrontage < (Q[2]+1.5*iqr), na.rm = TRUE)

dataTrain$SalePrice = SalePrice
dataTrain$LotFrontage = LotFrontage

boxplot(dataTrain$SalePrice ~ dataTrain$LotFrontage)
ggbetweenstats(dataTrain, SalePrice, LotFrontage, outlier.tagging = TRUE)
ggbetweenstats(eliminated, SalePrice, LotFrontage, outlier.tagging = TRUE)

# this works, need to fix how its storing the values 
for (col_name in names(dataTrainNum)) {
  outliers = boxplot(dataTrainNum[[col_name]], plot=FALSE, na.rm = TRUE)$out
  dataTrainNumOut = dataTrainNum[-which(dataTrainNum[[col_name]] %in% outliers),]
}

sum(is.na(eliminated$LotFrontage))


# normality testing
character_cols = character_columns

missing_columns = setdiff(names(dataTrain), names(dataTrainChar))
dataTrainNum <- dataTrain[, missing_columns]
# removes the sale price column
dataTrainNum$SalePrice = NULL
numeric_col = dataTrainNum

normality_tests <- list()

for (char_col in names(character_cols)) {
  char_levels <- unique(character_cols[[char_col]])
  char_test_results <- list()
  
  for (level in char_levels) {
    subset_data <- numeric_col[character_cols[[char_col]] == level]
    
    if (is.numeric(subset_data) && length(subset_data) >= 3) {
      shapiro_result <- shapiro.test(subset_data)
      char_test_results[[as.character(level)]] <- shapiro_result$p.value > 0.05
    } else {
      # Handle non-numeric data or subsets with less than 3 data points
      char_test_results[[as.character(level)]] <- NA
    }
  }
  
  normality_tests[[char_col]] <- char_test_results
}

# part 2 of checking for normality 
# Assuming your dataset is named 'data' and you want to compare numeric columns to 'SalePrice'
target_column <- dataTrain$SalePrice

# Identify numeric columns
numeric_columns <- sapply(dataTrain, is.numeric)

# Numeric columns to check for normality
columns_to_check_numeric <- names(dataTrain)[numeric_columns]

normality_results <- data.frame(Column = character(0), PValue = numeric(0))

for (col in columns_to_check_numeric) {
  # Perform normality checks as before
  shapiro_result <- shapiro.test(dataTrain[[col]])
  
  # Append the results to the new dataframe
  normality_results <- rbind(normality_results, data.frame(Column = col, PValue = shapiro_result$p.value))
  
  # Create QQ plot and histogram if desired
  qqnorm(dataTrain[[col]])
  qqline(dataTrain[[col]])
  hist(dataTrain[[col]], breaks = 20, main = paste("Histogram of", col))
  
  cat("Shapiro-Wilk Test for", col, "p-value:", shapiro_result$p.value, "\n\n")
}

# create excel/csv file for determining normality 