# sensitivityAnalysis
A shiny R web-based application for visualising one-way and two-way sensitivity analysis for cost-effectiveness analysis in medical decision-making

## The Problem
This is a web-based application that can be utilised in health care modelling and informatics for one-way and two-way sensitivity analysis when restructuring clinical, epidemiological and economic quantitative data into predictive and decision-making analyses for cost-effectiveness and health outcomes evaluations, to predict real-life conditions as closely as possible.

Sensitivity and uncertainty analysis have great value in aiding prediction of costs and outcomes of health interventions through modelling from quantitative clinical, epidemiological and economical data; parameter uncertainty determines the uncertainty of the model’s estimate since parameter values are inherently estimated. Varying parameter values to test the output sensitivity of the model identifies parameter uncertainty in important medical research and healthcare analyses. These include confidence intervals, cost-effectiveness analyses for medical technologies’ and interventions’ evaluation, cost-effectiveness acceptability curves, and need for and value of further information.

## Getting Started
These instructions will get you a copy of code running as an application on your local machine. The application can be viewed on your machine's browser. These instructions do not cover application deployment to the internet or any application distribution platform.

### Prerequisites
The application requires R version 3.5.2 (2018-12-20) to run.
The application can be deployed to an internet server. Find out more about building applications with Shiny here:http://shiny.rstudio.com/
This documentation will demonstrate how this application can be run from RStudio version 1.1.463 (2016).

The attached base packages are:
- stats
- graphics
- grDevices
- utils
- datasets
- methods
- base

The required packaged to install and their versions are:
- shiny version 1.2.0
- reshape2 version 1.4.3
- plyr version 1.8.4
- dplyr version 0.8.0.1
- ggplot2 version 3.1.1
- gridExtra version 2.3

Use \code{sessionInfo()} in the console in RStudio, to check versions, and attached and loaded packages in R.

### How to run the application
1. Open the file sensitivityAnalysis.R in R Studio.
2. Click the "Run App" button on the top right of the code window.

To display the code and the application side-by-side, use
\code{runApp(appDir = getwd(sensitivityAnalysis.R), display.mode = "showcase")} in the console in RStudio.

## Required User Inputs
This application allows the user to upload a .csv file with data loaded. An empty .csv file or non .csv file is read similarly to a corrupted or damaged file; the application will close to protect the server code.

Currently, this application can read and analyse .csv files where all data is numeric (headers do not have to be numeric, headers can be non-numeric).
This application can read .csv files with numeric and non-numeric values but the one-way and two-way sensitivity analyses can only be carried out on columns where all data is numeric only.
This application can read .csv files with non-numeric data but the data cannot be analysed.
Please see sampleData.csv file for an example of a data file that the application can read and analyse.

After uploading the file, please select if your data file has any headers, and type of data separators and quotations, on the left panel of the application. After each selection, click the "Submit" button to display the object.

### "Data" tab
Display drop down selector to choose how your uploaded data is displayed, just the top few rows "Head", or the entire data set "All"
Download PDF button will download the table or message displayed as a PDF file.

### "One-Way" tab
Select output drop down selector to choose which column the application should analyse as the output for one-way sensitivity analysis.
Download PDF button will download the tornado figure or message displayed as a PDF file.

### "Two-Way" tab
Select parameters and output drop down selectors to choose which columns the application should analyse as the two parameters and output for two-way sensitivity analysis. You must select a column for all three selectors, with no repetitions.
Download PDF button will download the contour plot or message displayed as a PDF file.

### "Summary" tab
A summary of the data file you have uploaded will be displayed here. Download PDF button will download the summary table or message displayed as a PDF file.

## Application Output
What do these diagrams mean?

### Tornado Plot
One-way sensitivity analysis produces a tornado plot.
The horizontal axis is the chosen output; along the vertical axis, parameters are arrayed and horizontal bars represent the outcome range associated with the specified parameter’s range. The outcome point estimate corresponding to base-case values is indicated by a vertical line cutting through all horizontal bars.

### Contour Plot
Two-way sensitivity analysis produces a contour plot.
The vertical axis represents values that your chosen 1st parameter take, and the horizontal axis represents values that the chosen 2nd parameter take. If you refer to the colour gradient key on the right, the colours of the individual points represent the outcome of the specified parameter values. The outcome point estimate corresponding to base-case values is indicated by the white coloured points.

## Downloaded Files
Refer to the sample PDF files for an example of what the displayed objects and downloaded PDF files should look like. They are named
- dataDisplayed.pdf
- oneWayAnalysis.pdf
- twoWayAnalysis.pdf
- summary.pdf
