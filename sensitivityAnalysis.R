#' This is a Shiny web application. You can run the application by clicking
#' the 'Run App' button above.
#'
#' Find out more about building applications with Shiny here:
#'
#'    http://shiny.rstudio.com/

# Loading and attaching add-on packages which will be used in the Shiny App
library(shiny)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotCostEffectiveness)
library(grDevices)
library(gridExtra)

# Some functions to display messages in \code{shiny} reports

#' Table message for exception
#' Typically call \code{return(tab_exception(...))} where you would have called \code{stop(...)}
#' @param ... text to display
#' @examples
#' tab_exception("no data for current filter selection")
#' tab_exception("This doesn't work!")
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

#' Plot message for exception
#' Typically call \code{return(plot_exception(...))} where you would have called \code{stop(...)}
#' @param ... text to display, concatenated with sep
#' @param sep separator used for concatenation
#' @param type function to use to print in console
#' @param color text color, by default red for message and warning else black
#' @param console if TRUE print in console, if FALSE just plot
#' @param size text size
#' @examples
#' plot_exception("no data for current filter selection")
#' plot_exception("NO","WAY","!!!",color="blue",size=12,console=FALSE)
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
  print(ggplot() +
          geom_text(ggplot2::aes(x=0,y=0,label=txt),color=color,size=size) +
          theme_void())
}

# Define the user interface, visual design of the page
ui <- fluidPage(
  
  # App title ----
  titlePanel( "Sensitivity Analysis for Medical Decision-Making"),
  
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
                  
                  # This tab will display one-way sensitivity anaylysis
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
                  
                  # This tab will display two-way sensitivity anaylysis
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
  
  #' Reactive expression to generate the requested outputs ----
  #' This is called whenever the inputs change. The output functions
  #' defined below then use the value computed from this expression.
  #' This has been set as NULL initially and will take on the data
  #' that the user has uploaded.
  uploaded <- reactiveValues(data = NULL)
  
  #' Reactive expression to generate the requested outputs ----
  #' This is called whenever the conditions change. The outputs to be
  #' displayed are then assigned to the values below. Values retrieved
  #' from this expression will then be displayed on the user interface.
  #' These have been set as NULL initially and will take on the table,
  #' plots and summary, according to the user input.
  reactiveDisplay <- reactiveValues(
    table = NULL,
    tornado = NULL,
    contour = NULL,
    summary = NULL)
  
  #' This responds to "event-like" reactive inputs
  #' input$uploadedfile is NULL initially
  observeEvent(input$uploadedfile, {
    if (is.null(input$uploadedfile))
      return(NULL)
    
    #' When reading semicolon separated files,
    #' having a comma separator causes `read.csv` to error
    #' This uses the user's selection of head, separator and quotations.
    tryCatch(
      {
        df <- read.csv(input$uploadedfile$datapath,
                       header = input$dataheader,
                       sep = input$datasep,
                       quote = input$dataquote)
      },
      error = function(e) {
        #' Return a safeError if a parsing error occurs
        #' This causes the app to close if file uploaded cannot be parsed
        stop(safeError(e))
      }
    )
    
    #' Parameter and output choices will be updated according to the column
    #' headers of the data file that has been uploaded by the user.
    #' User can then select which columns to be analysed for one-way
    #' analysis output and two-way analysis parameters and output
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
    
    #' Assign the extracted data from the user uploaded file to reactive
    #' value that had been set as NULL above
    uploaded$data <- df
  })
  
  #' Generate an HTML table view of the data
  #' This allows the user to view and check data that has been uploaded
  #' before the analysis plots are created ----
  output$datatable <- renderTable({
    
    # Handles an error if user has not uploaded any data.
    if (is.null(uploaded$data)){
      return(
        display <- tabException("You have not uploaded any data!")
        )
    }
    
    # Assigns uploaded data as a reactive value to a variable
    df <- uploaded$data
    
    #' Does not allow the user the use the analysis tool if data has less
    #' than 2 columns. For at least one-way analysis, 2 columns of data
    #' is necessary. Error message is raised.
    if(isTRUE(ncol(df) < 2 )){
      return(
        display <- tabException("Your data must have at least 2 columns.")
        )
    }
    
    #' Checks user input for table of data to be displayed, and assigns
    #' table to be displayed accordingly.
    if (input$datadisp == "head") {
      return(display <- head(df))
    }
    else {
      return(display <- df)
    }
    
    # Assign table to be displayed to reactive value to be reused.
    reactiveDisplay$table <- display
    
    # Reactive value is called to be displayed.
    reactiveDisplay$table
  })
  
  # Generate a PDF file of the data used that the user can download ----
  output$downloadDataTable <- downloadHandler(
    
    # Assign a filename
    filename = function() {
      paste("dataDisplayed", "pdf", sep = '.')
    },
    
    # Content to be included in the file
    content = function(file) {
      
      # Assign the table displayed from the reactive value to a variable
      saveTable <- reactiveDisplay$table
      
      pdf(file) # Open the device
      grid.table(saveTable) # Use grid.table() to print table to a PDF file
      dev.off() # Close the device
    })
  
  #' Generate plots of the data ----
  #' Also uses the inputs to build the plot label. Note that the
  #' dependencies on the inputs and the data reactive expression are
  #' both tracked, and all expressions are called in the sequence.
  #' 
  #' Generate one-way sensitivity analysis plot of the data ----
  output$onewayplot <- renderPlot({
    
    #' Handles error if user has not uploaded any data and ignores error
    #' message from the data table tab.
    if (is.null(uploaded$data)){
      return(
        display <- plotException("You have not uploaded any data!")
        )
    }
    
    # Assigns uploaded data as a reactive value to a variable
    df <- uploaded$data
    
    # Handles error if user has not chosen an output to be analysed.
    if (input$selectoutput1 == ""){
      return(
        display <- plotException("You have not chosen an output for analysis")
        )
    }
    
    #' Retrieves user input for output to be analysed, as character class,
    #' and assigns that to a variable
    output1 <- as.character(input$selectoutput1)
    
    # Separate output and non-output data into 2 data frames
    noOutput <- df %>% select(-matches(output1))
    onlyOutput <- df %>% select(matches(output1))
    
    # Rename the column of the output data to be easily retrieved
    names(onlyOutput)[1] <- "output"
    
    #' Identify the index of the minimum and maximum values of all non-
    #' output parameters
    minWhere <- sapply(noOutput, which.min)
    maxWhere <- sapply(noOutput, which.max)
    
    #' New data frame that with names of non-output parameters and their
    #' minimum and maximum output values using the indices retrieved on
    #' the data frame with only output values
    framedData <- data.frame(names = colnames(noOutput),
                             min = c(onlyOutput$output[minWhere]),
                             max = c(onlyOutput$output[maxWhere]))
    
    #' Data frame is melted into a vertical form that is more friendly to
    #' ggplot tornado function, to plot the sensitivity analysis.
    #' Data frame is then, sorted by non-output parameters, "names".
    meltedData <- melt(framedData, id.vars = "names",
                       variable.name = "val",
                       value.name = "output") %>%
      arrange(names)
    
    #' Class data as "tornado" and set specific attributes that
    #' plot function will take.
    class(meltedData) <- c("tornado", class(meltedData))
    attr(meltedData, "output_name") <- "output"
    
    #' Set baseline value of the output variable, median, and plot
    #' tornado using ggplot_tornado, from plotCostEffectiveness package.
    baseline_output <- median(onlyOutput$output, na.rm = TRUE)
    display <- ggplot_tornado(meltedData, baseline_output)
    
    # Assign plot to be displayed to reactive value to be reused.
    reactiveDisplay$tornado <- display
    
    # Reactive value is called to be displayed as a plot.
    reactiveDisplay$tornado
  })
  
  # Generate a PDF file of the tornado plot that the user can download ----
  output$downloadTornado <- downloadHandler(
    
    # Assign a file name
    filename = function() {
      paste("oneWayAnalysis", "pdf", sep = '.')
    },
    
    # Content to be included in the file
    content = function(file) {
      
      # Assign the plot displayed from the reactive value to a variable
      saveTornado <- reactiveDisplay$tornado
      
      pdf(file) # Open the device
      print(saveTornado) #Use print() to display ggplot on PDF file
      dev.off() # Close the device
    })
  
  # Generate two-way sensitivity analysis plot of the data ----
  output$twowayplot <- renderPlot({
    
    #' Handles error if user has not uploaded any data and ignores error
    #' message from the data table and one-way tab.
    if (is.null(uploaded$data)){
      return(
        display <- plotException("You have not uploaded any data!")
      )
    }
    
    # Assigns uploaded data as a reactive value to a variable
    df <- uploaded$data
    
    #' Handles error if user has not selected 2 parameters and an output
    #' for 2-way sensitivity analysis.
    if ((input$selectparam1 == "") |
        (input$selectparam2 == "") |
        (input$selectoutput2 == "")){
      return(
        display <- plotException(
          "You have to choose both parameters and the output for analysis.")
      )
    }
    
    #' Handles error if user has not selected different column headers
    #' for the 2 parameters and the output. All 3 selections must be
    #' different columns.
    #' Cannot have 2-way analysis if both parameters are the same.
    #' Cannot analyse a data set as both a parameter and an output.
    if ((input$selectparam1 == input$selectparam2) |
        (input$selectparam1 == input$selectoutput2) |
        (input$selectparam2 == input$selectoutput2)){
      return(
        display <- plotException("Cannot select same column more than once.")
        )
    }
    
    # Retrieve user inputs and assign to variables as characters.
    param1 <- as.character(input$selectparam1)
    param2 <- as.character(input$selectparam2)
    output2 <- as.character(input$selectoutput2)
    
    # Select data to be analysed as new data frame
    plotData <- df %>% select(matches(param1),
                              matches(param2),
                              matches(output2))
    
    # Rename the columns of analysis data to be easily retrieved
    names(plotData)[1] <- "parameter1"
    names(plotData)[2] <- "parameter2"
    names(plotData)[3] <- "output"
    
    #' Identify mid-point value of output data, median
    #' This will be used as the mid-point value in the plot
    midPointValue <- median(plotData$output, na.rm = TRUE)
    
    #' Use ggplot scatter plot and colour fill layering for
    #' 3 variable contour plot for 2-way analysis. Parameters selected
    #' will be the axes, output to be analysed as the colour fill.
    #' na.rm = TRUE included to handle any NULL values in the data.
    #' Plot has title and axes labelled according to user inputs of
    #' data selected as parameters and output in analysis. Themes, display
    #' and background are also chosen below.
    display <- ggplot(plotData,aes(parameter1, parameter2), na.rm = TRUE) +
      geom_point(aes(colour = output), na.rm = TRUE) +
      
      # continuous colours for the gradient
      scale_colour_gradient2(low = "blue", mid = "white",
                             high = "red", midpoint = midPointValue) +
      coord_equal() +
      theme_bw() +
      ggtitle("Two-way sensitivity analysis of", input$selectoutput2) +
      xlab(input$selectparam1) +
      ylab(input$selectparam2) +
      theme(panel.border = element_blank())
    
    # Assign plot to be displayed to reactive value to be reused.
    reactiveDisplay$contour <- display
    
    #Reactive value is called to be displayed as a plot
    reactiveDisplay$contour
  })
  
  # Generate a PDF file of the contour plot that the user can download ----
  output$downloadContour <- downloadHandler(
    
    # Assign a file name
    filename = function() {
      paste("twoWayAnalysis", "pdf", sep = '.')
    },
    
    # Content to be included in the file
    content = function(file) {
      
      # Assign the plot displayed from the reactive value to a variable
      saveContour <- reactiveDisplay$contour
      
      pdf(file) # Open the device
      print(saveContour) #Use print() to display ggplot on PDF file
      dev.off() # Close the device
    })
  
  # Generate a summary of the data ----
  output$datasummary <- renderPrint({
    
    #' Handles error if user has not uploaded any data and ignores error
    #' message from the previous three tabs
    if (is.null(uploaded$data)){
      return(
        display <- print("You have not uploaded any data!")
      )
    }
    
    # Assigns uploaded data as a reactive value to a variable
    df <- uploaded$data
    
    # Display summary of data user has uploaded
    display <- summary(df)
    
    # Assign object displayed to reactive value to be reused.
    reactiveDisplay$summary <- display
    
    #Reactive value is called to be displayed
    reactiveDisplay$summary
  })
  
  # Generate a PDF file of the data summary that the user can download ----
  output$downloadSummary <- downloadHandler(
    
    # Assign a filename
    filename = function() {
      paste("summary", "pdf", sep = '.')
    },
    
    # Content to be included in the file
    content = function(file) {
      
      # Assign the plot displayed from the reactive value to a variable
      saveSummary <- reactiveDisplay$summary
      
      pdf(file) # Open the device
      grid.table(saveSummary) # Use grid.table() to print summary to a PDF file
      dev.off() # Close the device
    })
}

# Create Shiny app ----
shinyApp(ui, server)
