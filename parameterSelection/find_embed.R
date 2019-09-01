# takes output from tseriesChaos::false.nearest()
# fnn_ts1 <- false.nearest(series, m, d, t, rt=10, eps=sd(series)/10)

find_embed <- function(fnn_ts1, fnn_ts2, delay, fnnpercent){
  # FNN calculation ----
  embdts1 = fnn_ts1
  embdts2 = fnn_ts2
  
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
  embdts2 = false.nearest(ts2, m = max.embed, d = delay, t = 0, rt = 10, 
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
  fnn_data_table <- tibble(ffn_ts1,ffn_ts2, "embed"=as.double(seq_along(ffn_ts1))) %>%
    gather(key = "timeseries",value = "percent_fnn", -embed)
  
  # create plot
  fnn_plot <- ggplot(fnn_data_table,aes(x = embed,y = percent_fnn,col=timeseries)) +
    geom_line() + 
    geom_vline(xintercept = embdim, linetype = "dotted") + 
    theme(axis.text.x = element_text(vjust = 0.5)) + 
    scale_x_continuous(labels = as.character(fnn_data_table$embed), breaks = fnn_data_table$embed)
  
  list("embed"=embdim,"embed_plot" = fnn_plot)
}



