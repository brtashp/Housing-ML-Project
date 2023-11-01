# automating data cleaning 

dataTrain = read.csv("train.csv") 

column_data_type = class(dataTrain[, 4])
print(column_data_type)

# Load the dplyr library for data manipulation
library(dplyr)

# Create a copy of the original data frame
dataTrain_copy <- dataTrain

# Find and process all character columns
character_columns <- sapply(dataTrain_copy, is.character)

for (col_name in names(dataTrain_copy)[character_columns]) {
  char_column_data <- dataTrain_copy %>%
    group_by({{col_name}}) %>%
    summarize(MeanSalePrice = mean(SalePrice)) %>%
    arrange(MeanSalePrice)
  
  char_to_numeric_mapping <- char_column_data %>%
    mutate(NumericValue = row_number()) %>%
    select({{col_name}}, NumericValue)
  
  dataTrain_copy <- dataTrain_copy %>%
    left_join(char_to_numeric_mapping, by = col_name)
}

# Print the updated data frame
print(dataTrain_copy)


# Create an empty data frame to store the summary statistics
summary_data <- data.frame(
  ColumnName = character(0),
  DataType = character(0),
  Mean = numeric(0),
  Median = character(0),
  StdDev = numeric(0),
  CorrelationWithSalePrice = numeric(0)
)

# Calculate the correlation with the "SalePrice" column
sale_price <- dataTrain$SalePrice
correlations <- sapply(dataTrain, function(x) {
  if (is.numeric(x)) {
    cor(x, sale_price)
  } else {
    NA
  }
})

# Loop through each column in 'dataTrain'
for (col_name in names(dataTrain)) {
  column_data <- dataTrain[[col_name]]
  
  # Determine the data type of the column
  col_type <- class(column_data)
  
  # Initialize variables for summary statistics
  col_mean <- NA
  col_median <- NA
  col_stddev <- NA
  
  if (col_type %in% c("integer", "numeric")) {
    col_mean <- mean(column_data)
    col_median <- median(column_data)
    col_stddev <- sd(column_data)
  } else if (col_type == "character") {
    # Handle character data type differently, e.g., count unique values
    unique_values <- length(unique(column_data))
    col_median <- paste("Unique Values:", unique_values)
  }
  
  # Calculate the correlation with the "SalePrice" column
  correlation <- correlations[col_name]
  
  # Create a data frame with the current column's information
  col_summary <- data.frame(
    ColumnName = col_name,
    DataType = col_type,
    Mean = col_mean,
    Median = col_median,
    StdDev = col_stddev,
    CorrelationWithSalePrice = correlation
  )
  
  # Append the current column's information to the summary data frame
  summary_data <- rbind(summary_data, col_summary)
}

# Print the summary_data data frame
print(summary_data)