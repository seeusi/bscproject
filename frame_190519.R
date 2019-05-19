# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
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
                  
                  # This tab will display the data as a table
                  tabPanel(title = "Data",
                           tableOutput("datatable")),
                  
                  # This tab will display one-way uncertainty anaylysis
                  # as a plot, depending on user input of the output
                  tabPanel(title = "One-way",
                           
                           # Input: Select output for analysis ----
                           selectInput(inputId = "selectoutput1",
                                        label = "Select outcome to be analysed",
                                        choices = c(),
                                        selected = NULL,
                                        multiple = FALSE),
                           plotOutput(outputId = "onewayplot")),
                  
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
                           plotOutput("twowayplot")),
                  
                  # This tab will display a summary of the data
                  tabPanel(title = "Summary",
                           verbatimTextOutput(outputId = "datasummary"))
      )
    )
  )
)


# Define the server code
server <- function(input, output, session){
  uploaded <- reactiveValues(data = NULL)
  
  observeEvent(input$uploadedfile, {
    if (is.null(input$uploadedfile))
      return(NULL)
    
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
    
    updateSelectInput(session, inputId = "selectoutput1",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput1))
    updateSelectInput(session, inputId = "selectparam1",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput1))
    updateSelectInput(session, inputId = "selectparam2",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput1))
    updateSelectInput(session, inputId = "selectoutput2",
                      choices = c(colnames(df)),
                      selected = paste(input$selectoutput1))
    
    uploaded$data <- df
  })
  
  output$datatable <- renderTable({
    if (is.null(uploaded$data)) 
      return(NULL)
    
    df <- uploaded$data
    
    if (input$datadisp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$onewayplot <- renderPlot({
    if (is.null(uploaded$data)) 
      return(NULL)
    
    df <- uploaded$data
    
    if (input$selectoutput1 == "")
      return(NULL)
    
    output1 <- as.character(input$selectoutput1)
    
    nooutput <- df %>% select(-matches(output1))
    onlyoutput <- df %>% select(matches(output1))
    
    names(onlyoutput)[1] <- "output"
    
    minwhere <- sapply(nooutput, which.min)
    maxwhere <- sapply(nooutput, which.max)
    
    frameddata <- data.frame(names = colnames(nooutput),
                             min = c(onlyoutput$output[minwhere]),
                             max = c(onlyoutput$output[maxwhere]))
    melteddata <- melt(frameddata, id.vars = "names",
                       variable.name = "val",
                       value.name = "output") %>%
      arrange(names)
    
    class(melteddata) <- c("tornado", class(melteddata))
    attr(melteddata, "output_name") <- "output"
    
    baseline_output <- median(onlyoutput$output, na.rm = TRUE)
    ggplot_tornado(melteddata, baseline_output)
  })
  
  output$twowayplot <- renderPlot({
    if (is.null(uploaded$data)) 
      return(NULL)
    
    df <- uploaded$data
    
    if ((input$selectparam1 == "") |
        (input$selectparam2 == "") |
        (input$selectoutput2 == "") |
        (input$selectparam1 == input$selectparam2) |
        (input$selectparam1 == input$selectoutput2) |
        (input$selectparam2 == input$selectoutput2))
      return(NULL)
    
    param1 <- as.character(input$selectparam1)
    param2 <- as.character(input$selectparam2)
    output2 <- as.character(input$selectoutput2)
    
    plotdata <- df %>% select(matches(param1),
                              matches(param2),
                              matches(output2))
    
    names(plotdata)[1] <- "parameter1"
    names(plotdata)[2] <- "parameter2"
    names(plotdata)[3] <- "output"
    midpointvalue <- mean(plotdata$output)
    
    ggplot(plotdata, aes(parameter1, parameter2), na.rm = TRUE) +
      geom_point(aes(colour = output), na.rm = TRUE) +
      # continuous colours for the gradient
      scale_colour_gradient2(low = "blue", mid = "white",
                             high = "red", midpoint = midpointvalue) +
      coord_equal() +
      theme_bw() +
      ggtitle("Two-way uncertainty analysis of", input$selectoutput2) +
      xlab(input$selectparam1) +
      ylab(input$selectparam2) +
      theme(panel.border = element_blank())
  })
  
  output$datasummary <- renderPrint({
    if (is.null(uploaded$data)) 
      return(NULL)
    
    df <- uploaded$data
    
    summary(df)
  })
}

# Create Shiny app ----
shinyApp(ui, server)