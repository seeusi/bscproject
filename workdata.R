library(rlist)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)

wd <- read.csv(here::here("mydata.csv"))
wdnooutput <- subset(wd, select = -output)
paramcount <- ncol(wdnooutput) #number of parameters, not counting the output column

min_values <- list()
max_values <- list()

for (column in wdnooutput){
  list.append(min_values, wd$output[which.min(column)])
  list.append(max_values, wd$output[which.max(column)])
}

minpar1 = which.min(wd[,1])
maxpar1 = which.max(wd[,1])
minpar2 = which.min(wd[,2])
maxpar2 = which.max(wd[,2])

reformdat <- data.frame(
  val = c("min",
          "max",
          "min",
          "max"),
  names = c("Parameter 1",
            "Parameter 1",
            "Parameter 2",
            "Parameter 2"),
  output = c(wd$output[minpar1], wd$output[maxpar1], wd$output[minpar1], wd$output[maxpar2])
)

class(reformdat) <- c("tornado", class(reformdat))
attr(reformdat, "output_name") <- "output"

baseline_output <- median(wd$output)
ggplot_tornado(reformdat, baseline_output)
