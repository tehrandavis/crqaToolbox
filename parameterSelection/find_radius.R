# # Set CRQA parameters ----
# min.rec = 2 # keep the RR constant
# max.rec = 2.5
# normalize = 1 # if normalize = 0 (do nothing); if normalize = 1 (Unit interval); if normalize = 2 (z-score)
# rescale = 2  # if rescale = 0 (do nothing); if rescale = 1 (mean distance of entire matrix); if rescale = 2 (maximum distance of entire matrix)          
# mindiagline = 2 # how many consecutive points constitutes a line?
# minvertline = 2 # same but for vertical lines (laminar)
# tw = 0 # leave this at 0; essentially how many diagonals off LOI to remove
# whiteline = FALSE
# recpt = FALSE
# 
# # set search scope parameters
# max.embed = 15
# max.delay = 60
# steps = seq(1, 6, 1)
# radiusspan = 200 
# radiussample = 80
# fnnpercent = 10
# typeami = "mindip"


find_radius <- function(ts1, 
                        ts2, 
                        min.rec, 
                        max.rec, 
                        embed,
                        delay,
                        steps,
                        radiusspan,
                        radiussample,
                        normalize, 
                        rescale, 
                        mindiagline = 2, 
                        minvertline = 2, 
                        tw = 0, 
                        whiteline = F, 
                        recpt = F
                        ){
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
  delay = delay
  embed = embed
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
  
  
  
  
  list("radius" = optrad, "radius_plot" = radi_plot)
}