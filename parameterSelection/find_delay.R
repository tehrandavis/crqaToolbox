library(peakPick)

find_delay <- function(ts1, ts2, max.delay, typeami, peak.limit = 10){
  mi1 = as.numeric(mutual(ts1, lag.max = max.delay, plot = FALSE)) %>% peakPick::peakpick(neighlim = peak.limit)
  mi2 = as.numeric(mutual(ts2, lag.max = max.delay, plot = FALSE)) %>% peakPick::peakpick(neighlim = peak.limit)
  # mi = ami(ts1, ts2, 1:max.delay)
  
  m1 = min(which(mi1==TRUE))
  m2 = min(which(mi2==TRUE))
  #m12 = min(mi)
  #mis = c(m1, m2, m12)
  mis = c(m1, m2)
  
  if (typeami == "mindip") {
    minmi = which(mis == min(mis))
    if (minmi == 1) 
      lag = mis[1]
    if (minmi == 2) 
      lag = mis[2]
    #if (minmi == 3) 
      #lag = which(mi == m12)
    maxlag = lag
  }
  if (typeami == "maxlag") {
    lg1 = m1
    lg2 = m2
    #lg12 = which(mi == m12)
    #lags = c(lg1, lg2, lg12)
    lags = c(lg1, lg2)
    maxlag = lags[which(lags == max(lags))]
  }
  if (length(maxlag) > 1) {
    maxlag = maxlag[sample(1:length(maxlag), 1)]
  }
  del = maxlag - 1
  
  # diagnostic plot for DELAY----
  mi1 <- mi1[1:max.delay]
  mi2 <- mi2[1:max.delay]
  #mi <- mi[1:max.delay]
  
  mutual_info_data_table <- tibble(mi1,mi2,"lag"=seq_along(mi1))
  mutual_info_data_table <- tidyr::gather(mutual_info_data_table, 
                                          key="timeseries", value="mutual_info", -lag)
  
  mutual_info_plot <- ggplot(mutual_info_data_table,
                             aes(x=lag,y=mutual_info,group=timeseries,
                                 col=timeseries)) +
    geom_line() + geom_vline(xintercept = del, linetype = "dotted") +
    ylab("mutual info")
  
  delay_out <- list("delay_plot"=mutual_info_plot, "delay"=del)
  return(delay_out)
}