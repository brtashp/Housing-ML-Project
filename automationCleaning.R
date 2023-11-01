# automating data cleaning 

dataTrain = read.csv("train.csv") 


column_data_type = class(dataTrain[, 4])
print(column_data_type)


# Create an empty data frame to store the summary statistics
summary_data <- data.frame(
  ColumnName = character(0),
  Mean = numeric(0),
  Median = numeric(0),
  StdDev = numeric(0)
)

# Loop through each column in 'dataTrain'
for (col_name in names(dataTrain)) {
  column_data <- dataTrain[[col_name]]
  
  # Calculate summary statistics
  col_mean <- mean(column_data)
  col_median <- median(column_data)
  col_stddev <- sd(column_data)
  
  # Create a data frame with the current column's information
  col_summary <- data.frame(
    ColumnName = col_name,
    Mean = col_mean,
    Median = col_median,
    StdDev = col_stddev
  )
  
  # Append the current column's information to the summary data frame
  summary_data <- rbind(summary_data, col_summary)
}

# Print the summary_data data frame
print(summary_data)
