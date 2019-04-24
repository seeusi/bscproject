library("dplyr")
library("ggplot2")

setwd("/Users/ongseeusi/Project")
wd <- read.csv("mydata.csv")

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
