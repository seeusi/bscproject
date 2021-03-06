# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinyWidgets)
library(reshape2)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)
library(grDevices)

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
      # pickerInput(inputId = "columnhead",
      #            label = "Filter Column by Header",
      #            choices = c(Column1 = "col1",
      #                        Column2 = "col2",
      #                        Column3 = "col3"),
      #            multiple = FALSE,
      #            options = c(title = "Select column to filter")),
      # sliderInput(inputId = "columnslide",
      #            label = "Filter Column Value",
      #            min = 0, max = 100, value = c(45, 55))
      # Submit button causes all the inputs on the page to not
      # send updates to the server until the button is pressed
      # submitButton("Submit")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset with data display, plots and data summary ----
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Data",
                           tableOutput("datatable")),
                  tabPanel(title = "One-way",
                            pickerInput(inputId = "selectoutput1",
                                       choices = c(),
                                       multiple = FALSE,
                                       options = c(title = "Select outcome to be analysed")),
                           plotOutput(outputId = "onewayplot")),
                  tabPanel(title = "Two-way",
                           pickerInput(inputId = "selectparam1",
                                       choices = c(),
                                       multiple = FALSE,
                                       options = c(title = "Select 1st parameter for analysis")),
                           pickerInput(inputId = "selectparam2",
                                       choices = c(),
                                       multiple = FALSE,
                                       options = c(title = "Select 2nd parameter for analysis")),
                           pickerInput(inputId = "selectoutput2",
                                       choices = c(),
                                       multiple = FALSE,
                                       options = c(title = "Select outcome to be analysed")),
                           plotOutput("twowayplot")),
                  tabPanel(title = "Summary",
                           verbatimTextOutput(outputId = "datasummary"),
                           
                           # Button to download all data produced
                           downloadButton(outputId = "downloadplots",
                                          label = "Download as PDF"))
      )
    )
  )
)

# Define the server code
server <- function(input, output, session){

  v <- reactiveValues(data = NULL)
  
  output$downloadplots <- downloadHandler(
    filename = function() {
      paste('plot', 'pdf', sep = '.')
    },
    content = function(file) {
      if (is.null(v$data)) 
        return(NULL)
      df <- v$data
      pdf(file) # open file
      # ggplot - doesn't work without print()
      # example below
      print(ggplot(df, aes(x = parameter1, y = parameter2)) +
        geom_line(colour = 'blue', size = 1) +
        geom_point(shape = 19, colour = 'red', size = 2))
      dev.off() # close file
    }
  )

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
    
    updatePickerInput(session, inputId = "selectoutput1",
                      choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectparam1",
                      choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectparam2",
                      choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectoutput2",
                      choices = c(colnames(df)))
    
    v$data <- df
  })

  output$datatable <- renderTable({

    if (is.null(v$data)) 
      return(NULL)

    df <- v$data

    if (input$datadisp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })
  
  output$onewayplot <- renderPlot({
    
    if (is.null(v$data))
      return(NULL)

    df <- v$data

    if (input$selectoutput1 == "")
      return(NULL)

    # nooutput <- df %>% select(-starts_with("output"))
    nooutput <- df %>% select(-starts_with(input$selectoutput1))
    
    minwhere <- sapply(nooutput, which.min)
    maxwhere <- sapply(nooutput, which.max)
    
    frameddata <- data.frame(names = colnames(nooutput),
                             min = c(df$output[minwhere]),
                             max = c(df$output[maxwhere]))
    melteddata <- melt(frameddata, id.vars = "names",
                       variable.name = "val",
                       value.name = "output") %>%
      arrange(names)
    
    class(melteddata) <- c("tornado", class(melteddata))
    attr(melteddata, "output_name") <- "output"
    
    baseline_output <- median(df$output, na.rm = TRUE)
    ggplot_tornado(melteddata, baseline_output)
  })
  
  output$twowayplot <- renderPlot({
    
    if (is.null(v$data)) 
      return(NULL)
    
    df <- v$data
    
    if ((input$selectparam1 == "") | (input$selectparam2 == "") | (input$selectoutput2 == ""))
      return(NULL)
    
    ggplot(df, aes(input$selectparam1, input$selectparam2, z = input$selectoutput2)) +
      geom_tile(aes(fill = input$selectoutput2)) +
      # scale_fill_manual(values = setNames(pal, levels(plot_data$INMB_cut))) +
      # scale_fill_gradientn(limits = c(-70, 35)) + #continuous colours
      coord_equal() +
      theme_bw() +
      xlab("Start (%)") +
      ylab("Complete (%)") +
      theme(panel.border = element_blank())
  })

  output$datasummary <- renderPrint({
    
    if (is.null(v$data)) 
      return(NULL)
    
    df <- v$data
    
    summary(df)
  })
}

# Create Shiny app ----
shinyApp(ui, server)