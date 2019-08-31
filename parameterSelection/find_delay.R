find_delay <- function(ts1, ts2, max.delay, typeami){
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
  
  # diagnostic plot for DELAY----
  mi1 <- mi1[1:max.delay]
  mi2 <- mi2[1:max.delay]
  mi <- mi[1:max.delay]
  
  mutual_info_data_table <- tibble(mi,mi1,mi2,"lag"=seq_along(mi))
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