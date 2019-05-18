library(reshape2)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)
library(mc2d)

setwd("/Users/ongseeusi/Project/bscproject")
plot_data <- read.csv("mydata.csv")

inmb_levelplot <-
  ggplot(plot_data, aes(parameter1, parameter2, z = output)) +
  geom_tile(aes(fill = output)) +
  # scale_fill_manual(values = setNames(pal, levels(plot_data$INMB_cut))) +
  # scale_fill_gradientn(limits = c(-70, 35)) + #continuous colours
  coord_equal() +
  theme_bw() +
  xlab("Start (%)") +
  ylab("Complete (%)") +
  theme(panel.border = element_blank())

print(inmb_levelplot)
