# takes output from tseriesChaos::false.nearest()
# fnn_ts1 <- false.nearest(series, m, d, t, rt=10, eps=sd(series)/10)

find_embed <- function(fnn_ts1, fnn_ts2, fnnpercent){
    # FNN calculation ----
    embdts1 = fnn_ts1
    embdts2 = fnn_ts2
    
    fnnfraction1 = embdts1[1, ]
    fnnfraction1 = fnnfraction1[which(is.na(fnnfraction1) == FALSE)]
    fnnfraction1_plot = fnnfraction1
    
    # get first local minimum:
    fnn_diffs1 <- diff(fnnfraction1) # find difference between embeds
    posdiff1 <- min(which(fnn_diffs1>0))-1 # check for monotonic decrease
    
    if (!is.infinite(posdiff1)){ # make sure it doesn't blow up if no min found
      end_fnn1 = posdiff1
    } else {
      end_fnn1 = length(fnnfraction1)
    }
    
    fnnfraction1 <- fnnfraction1[1:end_fnn1] # only consider embeds on decrease
    
    emdthd1 = fnnfraction1[1]/fnnpercent
    emdix1 = which(diff(fnnfraction1) < -emdthd1)
    if (length(emdix1) == 1) {
      emdmints1 = as.numeric(emdix1) + 1
    } else if (length(emdix1) > 1) {
      # find last embed that satisfies criteria
      emdmints1 = as.numeric(tail(emdix1, 1) + 1)
      # check two ahead just incase there is a dramatic decrease
      emdmints1_2out = emdmints1 + 2
      emdmints1_2diff <- fnnfraction1[emdmints1]-fnnfraction1[emdmints1_2out]
      emdmints1_2diff_num <- ifelse(is.na(emdmints1_2diff),0,emdmints1_2diff)
      
      # if two ahead is significant drop, use that, otherwise use original selection
      emdmints1 <- ifelse(emdmints1_2diff_num>emdthd1,emdmints1_2out,emdmints1)
    } else {
      emdmints1 = 1
    }
    
    fnnfraction2 = embdts2[1, ]
    fnnfraction2 = fnnfraction2[which(is.na(fnnfraction2) == FALSE)]
    fnnfraction2_plot = fnnfraction2
    
    
    # get first local minimum:
    fnn_diffs2 <- diff(fnnfraction2)
    posdiff2 <- min(which(fnn_diffs2>0))-1
    
    if (!is.infinite(posdiff2)){
      end_fnn2 = posdiff2
    } else {
      end_fnn2 = length(fnnfraction2)
    }
    fnnfraction2 <- fnnfraction2[1:end_fnn2]
    
    emdthd2 = fnnfraction2[1]/fnnpercent
    emdix2 = which(diff(fnnfraction2) < -emdthd2)
    
    if (length(emdix2) == 1) {
      emdmints2 = as.numeric(emdix2) + 1
    } else if (length(emdix2) > 1) {
      emdmints2 = as.numeric(tail(emdix2, 1) + 1)
      emdmints2_2out = emdmints2 + 2
      emdmints2_2diff <- fnnfraction2[emdmints2]-fnnfraction2[emdmints2_2out]
      emdmints2_2diff_num <- ifelse(is.na(emdmints2_2diff),0,emdmints2_2diff)
      emdmints2 <- ifelse(emdmints2_2diff_num>emdthd2,emdmints2_2out,emdmints2)
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
    max_ffn_dim <- ifelse(test = length(fnnfraction1_plot)==length(fnnfraction2_plot),
                          yes = length(fnnfraction1_plot), 
                          no = max(length(fnnfraction1_plot),length(fnnfraction2_plot))
    )
    
    ffn_ts1 <- rep(0, max_ffn_dim)
    ffn_ts1[1:length(fnnfraction1_plot)] <- fnnfraction1_plot
    
    ffn_ts2 <- rep(0, max_ffn_dim)
    ffn_ts2[1:length(fnnfraction2_plot)] <- fnnfraction2_plot
    
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
  