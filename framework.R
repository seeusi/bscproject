library(shiny)
library(rlist)
library(ggplot2)
library(dplyr)
library(mc2d)

###Develop webapp from within R
# Global variables can go here
# n1 <- 200


# Define the UI, visual design of the page
ui <- fluidPage(
  
  # App title ----
  titlePanel("Cost-effectiveness uncertainty analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", FALSE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("One-way", plotOutput("tornado")),
                  tabPanel("Two-way", plotOutput("heatmap")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define the server code
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
  thisdata <- reactive({
    
    # input$file1 will be NULL initially.
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    hist(runif(input$n1), col = blues9, border = blues9, labels = TRUE, main = "A random histogram", xlab = "The x-axis", ylab = "The y axis")
  })
    
  output$plot2 <- renderPlot({
    plot(runif(input$n1), col = blues9, border = blues9, labels = TRUE, main = "A random scatterplot", xlab = "The x-axis", ylab = "The y axis")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(thisdata())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    thisdata()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)