# automating data cleaning 

dataTrain = read.csv("train.csv") 

column_data_type = class(dataTrain[, 4])
print(column_data_type)

# Create an empty data frame to store the summary statistics
summary_data <- data.frame(
  ColumnName = character(0),
  DataType = character(0),
  Mean = numeric(0),
  Median = character(0), # Use character for mixed types
  StdDev = numeric(0)
)

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
  
  # Create a data frame with the current column's information
  col_summary <- data.frame(
    ColumnName = col_name,
    DataType = col_type,
    Mean = col_mean,
    Median = col_median,
    StdDev = col_stddev
  )
  
  # Append the current column's information to the summary data frame
  summary_data <- rbind(summary_data, col_summary)
}

# Print the summary_data data frame
print(summary_data)