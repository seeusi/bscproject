library(shiny)
library(shinyWidgets)
library(rlist)
library(ggplot2)
library(plotCostEffectiveness)
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
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      pickerInput(
        inputId = "sep", 
        label = "Separator", 
        choices = c(Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"), 
        multiple = FALSE,
        options = c(selected = ",")
      ),
      
      # Input: Select quotes ----
      pickerInput(
        inputId = "quote", 
        label = "Quote", 
        choices = c(None = "",
                    "Double Quote" = '"',
                    "Single Quote" = "'"), 
        multiple = FALSE,
        options = c(selected = "")
      ),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      pickerInput(
        inputId = "disp", 
        label = "Display", 
        choices = c(Head = "head",
                    All = "all"), 
        multiple = FALSE,
        options = c(selected = "head")
      ),
      
      # Input: Data filters ----
      pickerInput(
        inputId = "datfilt", 
        label = "Display", 
        choices = c(Head = "head",
                    All = "all"), 
        multiple = FALSE,
        options = c(selected = "head")
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("data")),
                  tabPanel("One-way",
                           pickerInput(
                             inputId = "outputPicker", 
                             label = "Multiple outcome datasets", 
                             choices = c("5 year survival", "QALYs", "Mortality", "list of health outcomes"), 
                             multiple = FALSE,
                             options = c(title = "Choose outcome to be analysed")
                           ),
                           plotOutput("plot")),
                  tabPanel("Two-way", plotOutput("plot2")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
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
    plot(plot.default(thisdata()))
  })
    
  output$plot2 <- renderPlot({
    plot(plot.default(thisdata()))
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(thisdata())
  })
  
  # Generate an HTML table view of the data ----
  output$data <- renderTable({
    if(input$disp == "head") {
      return(head(thisdata()))
    }
    else {
      return(thisdata())
  }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)