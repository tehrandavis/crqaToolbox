# crp_in_ggplot.R: takes output RQP from `crqa` package and plots using ggplot
# using scattermore function, plotting occurs much faster that geom_point for larger CRPs

# NEEDS:
#     - timeseries data for each ts in tibble form [time, position]
#     - crqa() output


# example workflow:

# required packages
library(crqa) # executes crqa
library(dplyr) # data wrangling (tidyverse)
library(ggplot2) # plotting
library(ggpubr) # adding border around plot
library(gridExtra) # placing multiple plots into grid
library(scattermore) #devtools::install_github('exaexa/scattermore')


# create two time series
ts1 = c(0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,1,0,0,
        0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
ts2 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,
        0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)

# set parameters for all CRQA analyses
delay = 1
embed = 1
rescale = 1
radius = 1
normalize = 0
mindiagline = 2
minvertline = 2
tw = 0
whiteline = FALSE
recpt = FALSE
side = "both"
checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)

# calculate CRQA
rec_analysis = crqa(ts1, ts2, delay, embed, rescale, radius, normalize, mindiagline, 
                    minvertline, tw, whiteline, recpt, side, checkl)


# ---- CRQA to CRP in ggplot

# rec_analysis is output from crqa()
RPData <- tibble("ts2" = rec_analysis$RP@i,
                 "ts1" = seq_along(rec_analysis$RP@i)
)
crp <- ggplot(data = RPData,mapping = aes(x = ts2,y = ts1)) + 
  geom_scattermore() + # could aslo geom_point (shape="." if you don't want to install scattermore)
  theme_minimal() +
  theme(axis.text =element_blank(),
        axis.title = element_blank()
  ) + 
  geom_abline(slope = max(RPData$ts1)/max(RPData$ts2), intercept = 0) + 
  border()

# timeseries plots of data (note that these ts come from rawdata
ts1_plotdata <- tibble("time" = seq_along(ts1), 
                       # need to make negative for flip:
                       "position" = ts1 * -1) # plot ends up on y-axis; so mult by -1 and coord_flip (see below)

ts2_plotdata <- tibble("time" = seq_along(ts2), 
                       "position" = ts2)

ts1_plot <- ggplot(data = ts1_plotdata, aes(x = time, y = position)) + 
  geom_line() +
  theme_light() +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.text.y=element_blank()
  ) + xlab("Time Series 1") + 
  coord_flip() 

ts2_plot <- ggplot(data = ts2_plotdata, aes(x = time, y = position)) + 
  geom_line() + 
  theme_light() +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.text.y=element_blank() 
  ) + xlab("Time Series 2")

# combine into one plot
ggarrange(ts1_plot, crp, NULL, ts2_plot, 
                      ncol = 2, nrow = 2,  align = "hv", 
                      widths = c(1, 4), heights = c(4, 1),
                      common.legend = TRUE
          )

# I recommend outputting to a 1-1 aspect ratio. For example:

library(cowplot)

cross_recurrence_plot <- ggarrange(ts1_plot, crp, NULL, ts2_plot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(1, 4), heights = c(4, 1),
          common.legend = TRUE
          ) 

cowplot::ggsave(filename = "cross_recurrence_plot.pdf", 
                plot = cross_recurrence_plot, 
                scale = 1, 
                width = 5, 
                height = 5, 
                units = "in")
