# lets use shiny
# Install and load necessary packages
if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
library(shiny)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Scrollable Table Example"),
  mainPanel(
    h3("Your Table Title"),
    DTOutput("scrollable_table")
  )
)

# Define the server
server <- function(input, output) {
  # Function to create scrollable table
  output$scrollable_table <- renderDT({
    datatable(
      dataTrain,  # Replace with your own dataframe
      options = list(
        scrollY = "200px",  # Set the height for the scrollable area
        paging = FALSE       # Disable paging for simplicity
      )
    )
  })
}

# Run the application
shinyApp(ui, server)

###############################################################################

# Define the UI
ui <- fluidPage(
  titlePanel("Summary Statistics for Categorical Features"),
  mainPanel(
    h3("Summary statistics for categorical features"),
    DTOutput("categorical_table")
  )
)

# Define the server
server <- function(input, output) {
  # Function to create summary statistics for categorical features
  output$categorical_table <- renderDT({
    categorical_features <- dataTrain[sapply(dataTrain, is.character)]
    cat_summary_stats <- summary(categorical_features)
    datatable(cat_summary_stats, options = list(paging = FALSE))
  })
}

# Run the application
shinyApp(ui, server)