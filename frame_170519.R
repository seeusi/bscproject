library(shiny)
library(shinyWidgets)
library(grDevices)
library(gridExtra)
library(datasets)
library(reshape2)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)

# Define the UI, visual design of the page
ui <- fluidPage(
  
  # App title ----
  titlePanel( "Cost-effectiveness uncertainty analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("uploadedfile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput("dataheader", "Header", TRUE),
      
      # Input: Select separator ----
      pickerInput(inputId = "datasep", 
                  label = "Separator", 
                  choices = c(Comma = ",",
                              Semicolon = ";",
                              Tab = "\t"), 
                  multiple = FALSE,
                  options = c(selected = ",")),
      
      # Input: Select quotes ----
      pickerInput(inputId = "dataquote", 
                  label = "Quote", 
                  choices = c(None = "",
                              "Double Quote" = '"',
                              "Single Quote" = "'"), 
                  multiple = FALSE,
                  options = c(selected = "")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select how data is displayed in data tab ----
      pickerInput(inputId = "datadisp", 
                  label = "Display", 
                  choices = c(Head = "head",
                              All = "all"), 
                  multiple = FALSE,
                  options = c(selected = "head"))
      
      # Input: Sliders for filtering data ----
      # Drop down box with names of the different column headers
      # Slider adjusts to value = median, min = min, max = max
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with data display, plots and data summary ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Data",
                           tableOutput("datatable")),
                  tabPanel(title = "One-way",
                           pickerInput(inputId = "selectoutput",
                                       choices = c(),
                                       multiple = FALSE,
                                       options = c(title = "Select outcome to be analysed")
                                       ),
                           plotOutput(outputId = "onewayplot")),
                  tabPanel(title = "Two-way",
                           plotOutput("twowayplot")),
                  tabPanel(title = "Summary",
                           verbatimTextOutput(outputId = "datasummary"),
                           
                           # Button to download all data produced
                           downloadButton(outputId = "downloadplots",
                                          label = "Download"))
  )
)
)
)

# Define the server code
server <- function(input, output){
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  reactivedata <- reactive({
    if (is.null(inFile$datapath)) {
      dat <- read.csv("path/to/your.csv")
      values$file_name = "your.csv"
      values$file_type = "csv"
      values$file_size = file.size("path/to/your.csv")
      values$file_path = "path/to/your.csv"
    } else {
      dat <- read.csv(inFile$datapath)
      values$file_name = inFile$name
      values$file_size = inFile$size
      values$file_type = inFile$type
      values$file_path = inFile$datapath
    }
  })
  reactivedata <- reactive({
    
    # input$uploadedfile is NULL initially
    req(input$uploadedfile)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$uploadedfile$datapath,
                       header = input$dataheader,
                       sep = input$datasep,
                       quote = input$dataquote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  # Generate an HTML table view of the data ----
  output$datatable <- renderTable({
    if(input$datadisp == "head") {
      return(head(reactivedata))
    }
    else {
      return(reactivedata)
    }
  })
  
  # Generate a summary of the data ----
  output$datasummary <- renderPrint({
    summary(reactivedata)
  })
  
  # Downloadable pdf of data plots and summary ----
  output$downloadplots <- downloadHandler(
    filename = function() {"diagrams.pdf"},
    content = function(file) {
      pdf(file, onefile = TRUE)
      table({
        if(input$datadisp == "head") {
          return(head(reactivedata()))
        }
        else {
          return(reactivedata())}
      })
      dev.off()
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)