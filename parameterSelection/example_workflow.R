# note that this requires the `ami()` function from Coco's `crqa` package. 
# A direct link to the source:
# https://raw.githubusercontent.com/cran/crqa/master/R/ami.R
# code also included in this repo

# the functions find_delay, find_embed, find_radius derived from Coco's optimizeParameters function

library(tidyverse)
library(crqa)
source("https://raw.githubusercontent.com/cran/crqa/master/R/ami.R") #ami
source("parameterSelection/find_delay.R")
source("parameterSelection/find_embed.R")
source("parameterSelection/find_radius.R")

# get the timeseries data ----
exampleData <- readr::read_csv("parameterSelection/exampleData.csv")

ts1 <- exampleData$person1_ts
ts2 <- exampleData$person2_ts

# Set CRQA parameters ----
min.rec = 2.25 # keep the RR constant
max.rec = 2.5
normalize = 1 # if normalize = 0 (do nothing); if normalize = 1 (Unit interval); if normalize = 2 (z-score)
rescale = 1  # if rescale = 0 (do nothing); if rescale = 1 (mean distance of entire matrix); if rescale = 2 (maximum distance of entire matrix)          
mindiagline = 2 # how many consecutive points constitutes a line?
minvertline = 2 # same but for vertical lines (laminar)
tw = 0 # leave this at 0; essentially how many diagonals off LOI to remove
whiteline = FALSE
recpt = FALSE

# set search scope parameters
max.embed = 10
max.delay = 60
steps = seq(1, 6, 1)
radiusspan = 200 
radiussample = 80
fnnpercent = 10
typeami = "mindip"

# calculate delay
delay_data <- find_delay(ts1, ts2, max.delay,typeami)

# calculate FNN
ffn_ts1 <- tseriesChaos::false.nearest(series = ts1, m = max.embed, d = delay_data$delay, 
                                       t = 0,rt = 10, eps = sd(ts1)/10)

ffn_ts2 <- tseriesChaos::false.nearest(series = ts2, m = max.embed, d = delay_data$delay, 
                                       t = 0,rt = 10, eps = sd(ts2)/10)

embed_data <- find_embed(ffn_ts1,ffn_ts2, fnnpercent)

# calculate radius
radius_data <- find_radius(ts1,ts2, min.rec,max.rec,
                           embed = embed_data$embed,
                           delay = delay_data$delay,
                           steps,radiusspan,radiussample,
                           normalize,rescale)


# DIAGNOSTICS -----
mutual_info_plot <- delay_data$delay_plot
fnn_plot <- embed_data$embed_plot
radi_plot <- radius_data$radius_plot

diagnostic_plots <- cowplot::plot_grid(mutual_info_plot,fnn_plot,radi_plot,
                                       nrow = 3,
                                       align = "h",
                                       labels = c("delay","embed","radius")
)


cowplot::ggsave2(filename = "parameterSelection/diagnostic_plots.pdf",
                 plot = diagnostic_plots,
                 scale = 1,
                 width = 6,
                 height = 6,
                 units = "in",
                 dpi = 300)

