library(reshape2)
library(ggplot2)
library(plotCostEffectiveness)
library(dplyr)
library(mc2d)

setwd("/Users/ongseeusi/Project/bscproject")
mydata <- read.csv("mydata.csv")
plotdata <- mydata %>% select(matches("parameter1"),
                              matches("parameter2"),
                              matches("output"))
names(plotdata)[1] <- "p1"
names(plotdata)[2] <- "p2"
names(plotdata)[3] <- "op"
mid <- mean(plotdata$op)

inmb_levelplot <-
  ggplot(plotdata, aes(p1, p2), na.rm = TRUE) +
  geom_point(aes(colour = op), na.rm = TRUE) +
  # scale_fill_manual(values = setNames(pal, levels(plot_data$INMB_cut))) +
  scale_colour_gradient2(low = "red", mid = "white",
                         high = "blue", midpoint = mid) +
  coord_equal() +
  theme_bw() +
  ggtitle("two") +
  xlab("Start (%)") +
  ylab("Complete (%)") +
  theme(panel.border = element_blank())

print(inmb_levelplot)
