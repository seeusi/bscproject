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

# Define the functions
# Can shift this to an import so this file focuses on the app
get_df_from_input <- function(input) {
  
  # input$uploadedfile will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
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
  
  return(df)

}

datatable <- function(df, input, output) {
  renderTable({
    if(input$datadisp == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })
}

datasummary <- function(df, input, output) {
  renderPrint({
    summary(df)
  })
}

onewayplot <- function(df, input, output) {
  renderPlot({
    
    nooutput <- df %>% select(-matches(input$selectoutput1))
    
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
}

twowayplot <- function(df, input, output) {
  renderPlot({
    ggplot(df, aes_(x = input$selectparam1, y = input$selectparam2, z = input$selectoutput2)) +
      geom_tile(aes(fill = input$selectoutput2)) +
      # scale_fill_manual(values = setNames(pal, levels(plot_data$INMB_cut))) +
      # scale_fill_gradientn(limits = c(-70, 35)) + #continuous colours
      coord_equal() +
      theme_bw() +
      xlab("Start (%)") +
      ylab("Complete (%)") +
      theme(panel.border = element_blank())
  })
}

# Define Variables
df.cache <- NULL

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
  
  # Handle new file
  observeEvent({
    
    # When there is a change to any of these inputs,
    input$uploadedfile
    input$dataheader
    input$datasep
    input$dataquote
  
  }, {
    
    # 1. load a df from the file
    df <- get_df_from_input(input)
    assign("df.cache", df, envir = .GlobalEnv)
    
    # 2. update all pickers that depend on df
    updatePickerInput(session, inputId = "selectoutput1", choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectoutput2", choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectparam1", choices = c(colnames(df)))
    updatePickerInput(session, inputId = "selectparam2", choices = c(colnames(df)))
    
    # 3. update all outputs that depend on df
    output$datatable <- datatable(df, input, output)
    output$datasummary <- datasummary(df, input, output)
    
  })
  
  # Handle one way plot
  observeEvent({
    
    input$selectoutput1
  
  }, {
    
    df <- get("df.cache", envir = .GlobalEnv)
    output$onewayplot <- onewayplot(df, input, output)
  
  })
  
  # Handle two way plot
  observeEvent({
    
    input$selectparam1
    input$selectparam2
    input$selectoutput2
    
  }, {
    
    df <- get("df.cache", envir = .GlobalEnv)
    output$twowayplot <- twowayplot(df, input, output)
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)