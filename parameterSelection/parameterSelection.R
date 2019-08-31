# note that this requires the `ami()` function from Coco's `crqa` package. A direct link to the source:
# https://raw.githubusercontent.com/cran/crqa/master/R/ami.R
# code also included in this repo

library(tidyverse)
library(crqa)
source("https://raw.githubusercontent.com/cran/crqa/master/R/ami.R")

# get the timeseries data ----
exampleData <- readr::read_csv("parameterSelection/exampleData.csv")

ts1 <- exampleData$person1_ts
ts2 <- exampleData$person2_ts

# Set CRQA parameters ----
min.rec = 2 # keep the RR constant
max.rec = 2.5
normalize = 1 # if normalize = 0 (do nothing); if normalize = 1 (Unit interval); if normalize = 2 (z-score)
rescale = 2  # if rescale = 0 (do nothing); if rescale = 1 (mean distance of entire matrix); if rescale = 2 (maximum distance of entire matrix)          
mindiagline = 2 # how many consecutive points constitutes a line?
minvertline = 2 # same but for vertical lines (laminar)
tw = 0 # leave this at 0; essentially how many diagonals off LOI to remove
whiteline = FALSE
recpt = FALSE

# set search scope parameters
max.embed = 15
max.delay = 60
steps = seq(1, 6, 1)
radiusspan = 200 
radiussample = 80
fnnpercent = 10
typeami = "mindip"


# what follows is an extension of `optimizeParam` from Coco's `crqa` package

# find delay using AMI ----
mi1 = as.numeric(mutual(ts1, lag.max = max.delay, plot = FALSE))
mi2 = as.numeric(mutual(ts2, lag.max = max.delay, plot = FALSE))
mi = ami(ts1, ts2, 1:max.delay)
m1 = min(mi1)
m2 = min(mi2)
m12 = min(mi)
mis = c(m1, m2, m12)

if (typeami == "mindip") {
  minmi = which(mis == min(mis))
  if (minmi == 1) 
    lag = which(mi1 == m1)
  if (minmi == 2) 
    lag = which(mi2 == m2)
  if (minmi == 3) 
    lag = which(mi == m12)
  maxlag = lag
}
if (typeami == "maxlag") {
  lg1 = which(mi1 == m1)
  lg2 = which(mi2 == m2)
  lg12 = which(mi == m12)
  lags = c(lg1, lg2, lg12)
  maxlag = lags[which(lags == max(lags))]
}
if (length(maxlag) > 1) {
  maxlag = maxlag[sample(1:length(maxlag), 1)]
}
del = maxlag - 1

# FNN calculation ----
embdts1 = false.nearest(ts1, m = max.embed, d = del, t = 0, rt = 10, # as of 1.0.8 `Coco's crqa has error here, d=1`
                        eps = sd(ts1)/10)
fnnfraction1 = embdts1[1, ]
fnnfraction1 = fnnfraction1[which(is.na(fnnfraction1) == 
                                    FALSE)]
emdthd1 = fnnfraction1[1]/fnnpercent
emdix1 = which(diff(fnnfraction1) < -emdthd1)
if (length(emdix1) == 1) {
  emdmints1 = as.numeric(emdix1) + 1
  } else if (length(emdix1) > 1) {
  emdmints1 = as.numeric(tail(emdix1, 1) + 1)
  } else {
  emdmints1 = 1
}
embdts2 = false.nearest(ts2, m = max.embed, d = del, t = 0, rt = 10, 
                        eps = sd(ts2)/10)
fnnfraction2 = embdts2[1, ]
fnnfraction2 = fnnfraction2[which(is.na(fnnfraction2) == 
                                    FALSE)]
emdthd2 = fnnfraction2[1]/fnnpercent
emdix2 = which(diff(fnnfraction2) < -emdthd2)
if (length(emdix2) == 1) {
  emdmints2 = as.numeric(emdix2) + 1
} else if (length(emdix2) > 1) {
  emdmints2 = as.numeric(tail(emdix2, 1) + 1)
} else {
  emdmints2 = 1
}

if (length(emdmints1) > 1) {
  emdmints1 = emdmints1[1]
}
if (length(emdmints2) > 1) {
  emdmints2 = emdmints2[1]
}
embdim = max(c(emdmints1, emdmints2))





# find RADIUS
dm = rdist(ts1, ts2)
if (rescale > 0) {
  switch(rescale, {
    1
    rescaledist = mean(dm)
    dmrescale = (dm/rescaledist) * 100
  }, {
    2
    rescaledist = max(dm)
    dmrescale = (dm/rescaledist) * 100
  })
} else {
  dmrescale = dm
}
combo = c(ts1, ts2)
sdun = sd(dmrescale)
mnun = median(dmrescale) * 3
radi = seq(mnun, 0, -(sdun/radiusspan))
delay = del
embed = embdim
optrad = vector()
end.flag <- 0

while (end.flag == 0) {
  hi.loc <- 1
  lo.loc <- length(radi)
  curr.loc <- round(length(radi)/2)
  r <- radi[curr.loc]
  res = crqa(ts1, ts2, delay, embed, rescale, r, normalize, 
             mindiagline, minvertline, tw, whiteline, recpt)
  if (res$RR >= min.rec & res$RR <= max.rec) {
    optrad = r
    end.flag <- 1
  }
  else {
    if (res$RR < min.rec) {
      lo.loc <- curr.loc
    }
    if (res$RR > max.rec) {
      hi.loc <- curr.loc
    }
    if ((lo.loc - hi.loc) < 2) {
      end.flag <- 1
      warning("Optimal Radius Not found: try again choosing a wider radius span and larger sample size")
    }
  }
  radi <- radi[hi.loc:lo.loc]
}
if (length(optrad) == 0) {
  optrad = NA
}


rec_parms <- tibble("radius" = optrad, "embed" = embdim, "delay" = del)


# DIAGNOSTIC PLOTS ----

# diagnostic plot for DELAY----
mi1 <- mi1[1:max.delay]
mi2 <- mi2[1:max.delay]
mi <- mi[1:max.delay]

mutual_info_plot <- tibble(mi1,mi2,mi,"lag"=seq_along(mi)) %>% 
  tidyr::gather(., key="timeseries", value="mutual_info", -lag) %>% 
  ggplot(aes(x=lag,y=mutual_info,col=timeseries)) + 
  geom_line() + geom_vline(xintercept = del, linetype = "dotted") +
  ylab("mutual info")

# diagnostic plot for EMBED----

# pad the two fnn series at same length
max_ffn_dim <- ifelse(test = length(fnnfraction1)==length(fnnfraction2),
                      yes = length(fnnfraction1), 
                      no = max(length(fnnfraction1),length(fnnfraction2))
)

ffn_ts1 <- rep(0, max_ffn_dim)
ffn_ts1[1:length(fnnfraction1)] <- fnnfraction1

ffn_ts2 <- rep(0, max_ffn_dim)
ffn_ts2[1:length(fnnfraction2)] <- fnnfraction2

# create table
fnn_table <- tibble(ffn_ts1,ffn_ts2, "embed"=as.double(seq_along(ffn_ts1))) %>%
  gather(key = "timeseries",value = "percent_fnn", -embed)

# create plot
fnn_plot <- ggplot(fnn_table,aes(x = embed,y = percent_fnn,col=timeseries)) +
  geom_line() + 
  geom_vline(xintercept = embdim, linetype = "dotted") + 
  theme(axis.text.x = element_text(vjust = 0.5)) + 
  scale_x_continuous(labels = as.character(fnn_table$embed), breaks = fnn_table$embed)


# diagnose RADIUS ----

# get values from around selected radius
radi_range <- seq(optrad-5,optrad+5,.5) %>% round(digits=2)
rec_rates <- vector()

for (i in 1:length(radi_range)){
  r <- radi_range[i]
  res = crqa(ts1, ts2, delay, embed, rescale, r, normalize, 
             mindiagline, minvertline, tw, whiteline, recpt)
  rec_rates[i] <- res$RR
}

radi_table <- tibble(radi_range, rec_rates)

radi_plot <- ggplot(radi_table, aes(x = radi_range, y = rec_rates)) +
  geom_point() + 
  geom_line() + 
  geom_vline(xintercept = optrad, lty="dotted") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5)) + 
  scale_x_continuous(labels = as.character(radi_table$radi_range), breaks = radi_table$radi_range)


diagnostic_plots <- cowplot::plot_grid(mutual_info_plot,fnn_plot,radi_plot,
                                       nrow = 3,
                                       align = "h",
                                       labels = c("delay","embed","radius")
                                       )

# save plots  
cowplot::ggsave(filename = "parameterSelection/parameter_diagnositics.pdf",
                  plot = diagnostic_plots, 
                  scale = 1, 
                  width = 6, height = 6, 
                  units = "in", dpi = 300)


