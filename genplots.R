library(reshape2)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)
library(mc2d)

setwd("/Users/ongseeusi/Project/bscproject")
originaldata <- read.csv("mydata.csv")

nooutput <- subset(originaldata, select = -output)
minwhere <- sapply(nooutput, which.min)
maxwhere <- sapply(nooutput, which.max)

frameddata <- data.frame(names = colnames(nooutput),
                         min = c(originaldata$output[minwhere]),
                         max = c(originaldata$output[maxwhere]))
melteddata <- melt(frameddata, id.vars = "names",
                   variable.name = "val",
                   value.name = "output") %>%
  arrange(names)

class(melteddata) <- c("tornado", class(melteddata))
attr(melteddata, "output_name") <- "output"

baseline_output <- median(originaldata$output, na.rm = TRUE)
ggplot_tornado(melteddata, baseline_output)

## model ouput ##
s_analysis <- model.frame(formula = output ~. ,
                          data = originaldata)
s_analysis %>%
  s_analysis_to_tornado_plot_data %>%
  ggplot_tornado(baseline_output = median(originaldata$output, na.rm = TRUE))
