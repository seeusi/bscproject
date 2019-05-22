library(shiny)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotCostEffectiveness)
library(grDevices)
library(gridExtra)

tabException <-function(
  ...){
  
  # This converts plain text into "character" class
  txt = paste(...)
  
  # Text input is converted into a data frame with column head, "ISSUE"
  df = data.frame("ISSUE" = txt)
  
  # Text input returned as a data frame, which can be displayed by renderTable
  return(df)
  invisible(NULL)
}

plotException <-function(
  ...,
  sep=" ", 
  type=c("message","warning","cat","print"),
  color="auto",
  console=FALSE,
  size = 6){      
  type=match.arg(type)
  
  # Plain text converted into character class
  txt = paste(...,collapse=sep)
  
  # Controls and conditions to pass into the console
  if(console){
    if(type == "message") message(txt)
    if(type == "warning") warning(txt)
    if(type == "cat") cat(txt)
    if(type == "print") print(txt)
  }
  
  # Default colour
  if(color =="auto") color <- if(type == "cat") "black" else "red"
  if(txt == "warning") txt <- paste("warning:",txt)
  
  # Text input returned as a plot, which can be displayed by renderPlot
  print(ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=txt),color=color,size=size) + 
          ggplot2::theme_void())
  invisible(NULL)
}

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
      selectInput(inputId = "datasep", 
                  label = "Separator", 
                  choices = c(Comma = ",",
                              Semicolon = ";",
                              Tab = "\t"), 
                  selected = ",",
                  multiple = FALSE),
      
      # Input: Select quotes ----
      selectInput(inputId = "dataquote", 
                  label = "Quote", 
                  choices = c(None = "",
                              "Double Quote" = '"',
                              "Single Quote" = "'"),
                  selected = "",
                  multiple = FALSE),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select how data is displayed in data tab ----
      selectInput(inputId = "datadisp", 
                  label = "Display", 
                  choices = c(Head = "head",
                              All = "all"),
                  selected = "head",
                  multiple = FALSE),
      
      # Submit button causes all the inputs on the page to not
      # send updates to the server until the button is pressed
      submitButton("Submit")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with data display, plots and data summary ----
      tabsetPanel(type = "tabs",
                  
                  # This tab will display the uploaded data as a table
                  tabPanel(title = "Data",
                           tableOutput("datatable"),
                           
                           # Button to download head of data table used as PDF
                           downloadButton(outputId = "downloadDataTable",
                                          label = "Download as PDF")),
                  
                  # This tab will display one-way uncertainty anaylysis
                  # as a plot, depending on user input of the output
                  tabPanel(title = "One-way",
                           
                           # Input: Select output for analysis ----
                           selectInput(inputId = "selectoutput1",
                                       label = "Select outcome to be analysed",
                                       choices = c(),
                                       selected = NULL,
                                       multiple = FALSE),
                           plotOutput(outputId = "onewayplot"),
                           
                           # Button to download plot produced as PDF
                           downloadButton(outputId = "downloadTornado",
                                          label = "Download as PDF")),
                  
                  # This tab will display two-way uncertainty anaylysis
                  # as a plot, depending on user input of parameters and output
                  tabPanel(title = "Two-way",
                           
                           # Input: Select first parameter for analysis ----
                           selectInput(inputId = "selectparam1",
                                       label = "Select first parameter",
                                       choices = c(),
                                       selected = NULL,
                                       multiple = FALSE),
                           
                           # Input: Select second parameter for analysis ----
                           selectInput(inputId = "selectparam2",
                                       label = "Select second parameter",
                                       choices = c(),
                                       selected = NULL,
                                       multiple = FALSE),
                           
                           # Input: Select output for analysis ----
                           selectInput(inputId = "selectoutput2",
                                       label = "Select output to be analysed",
                                       choices = c(),
                                       selected = NULL,
                                       multiple = FALSE),
                           
                           plotOutput("twowayplot"),
                           
                           # Button to download plot produced as PDF
                           downloadButton(outputId = "downloadContour",
                                          label = "Download as PDF")),
                  
                  # This tab will display a summary of the data
                  tabPanel(title = "Summary",
                           verbatimTextOutput(outputId = "datasummary"),
                           
                           # Button to download summary of the data as PDF
                           downloadButton(outputId = "downloadSummary",
                                          label = "Download as PDF"))
      )
    )
  )
)


# Define the server code
server <- function(input, output, session){
  reactiveItem <- reactiveValues(
    data = NULL,
    displayTable = NULL,
    displayTornado = NULL,
    displayContour = NULL,
    displaySummary = NULL)
  
  observeEvent(input$uploadedfile, {
    if (is.null(input$uploadedfile)){
      return(NULL)
      }
    tryCatch(
      {
        df <- read.csv(input$uploadedfile$datapath,
                       header = input$dataheader,
                       sep = input$datasep,
                       quote = input$dataquote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    updateSelectInput(session, inputId = "selectoutput1",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput1))
    updateSelectInput(session, inputId = "selectparam1",
                      choices = c(colnames(df)),
                      selected = paste(input$selectparam1))
    updateSelectInput(session, inputId = "selectparam2",
                      choices = c(colnames(df)),
                      selected = paste(input$selectparam2))
    updateSelectInput(session, inputId = "selectoutput2",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput2))
    
    reactiveItem$data <- df
    })
  
  output$datatable <- renderTable({
    if (is.null(input$uploadedfile)){
      return(
        reactiveItem$displayTable <- tabException("You are lousy!")
      )}
    reactiveItem$displayTable
  })
  
  output$onewayplot <- renderPlot({
    reactiveItem$displayTornado
  })
}

# Create Shiny app ----
shinyApp(ui, server)