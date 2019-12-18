get_general_stats_in_cycle <- function (cycles)
{
  block_size <- jmax*kmax*lmax
  counts <- matrix(nrow = imax, ncol = length(cycles)*(3+lmax))   # 1 for total size, 1 for male count, 1 for accumulated fatal overdose and lmax for all oud states
  
  for (i in 1:imax) # for each block
  {
    it <- 1
    # total size
    for (cycle in cycles)   
    {
      counts[i,it] <- sum(out$general_outputs[cycle+1,which(substring(general_IDs, 2, 3) == block_idx[i])])
      it <- it+1
    }
    
    # male counts
    for (cycle in cycles)   
    {
      counts[i,it] <- sum(out$general_outputs[cycle+1,(which(substring(general_IDs, 2, 3) == block_idx[i] & substring(general_IDs,6,7) == sex_idx[1]))])
      it <- it+1
    }
    
    # oud counts
    for (l in 1:lmax)
    {
      for (cycle in cycles)   
      {
        counts[i,it] <- sum(out$general_outputs[cycle+1,(which(substring(general_IDs, 2, 3) == block_idx[i] & substring(general_IDs,8,9) == oud_idx[l]))])
        it <- it+1
      }
    }
    
    # fatal overdose counts
    # check if fatal od ratio changes within time intervals. If so, throw an error.
    if (!(length(general_stats_cycles) == 1 & general_stats_cycles[1]==0))
    {
      tmp_cycles <- cycles
      if (!0 %in% general_stats_cycles)
      {
        tmp_cycles <- c(0,cycles)
      }
      
      for (t in time_varying_overdose_cycles)
      {
        for (ii in 1:(length(tmp_cycles)-1))
        {
          if (tmp_cycles[ii]+1 <= t & t < tmp_cycles[ii+1])
          {
            stop("Change in fatal overdoses ratios cannot happen between output time intervals!")
          }
        }
      }
    }
    it2 <- 0
    for (cycle in cycles)
    { 
      if (cycle == 0)
      {
        counts[i,it] <- 0
      } else {
        # get index of time-varying interval
        f_od_idx <- -1
        for (t in 1:length(time_varying_overdose_cycles))
        {
          if (f_od_idx == -1 & cycle <= time_varying_overdose_cycles[t])
          {
            f_od_idx <- t
          }
        }
        counts[i,it] <- sum(out$overdose_outputs[(it2+2):(cycle+1),which(substring(active_oud_IDs, 2, 3) == block_idx[i])]) * fatal_overdose_vec[f_od_idx]
        it2 <- cycle      
      }
      it <- it+1
    }
  } # for all blocks
  
    it <- 1
    col_names <- rep("",length(cycles)*(3+lmax))
    for (cycle in cycles)
    {
      col_names[it] <- paste("total_size_cycle",cycle,sep="") 
      it <- it+1
    }
    for (cycle in cycles)
    {
      col_names[it] <- paste("male_counts_cycle",cycle,sep="") 
      it <- it+1
    }
    for (l in 1:lmax)
    {
      for (cycle in cycles)
      {
        col_names[it] <- paste(oud[l],"_cycle",cycle,sep="") 
        it <- it+1
      }
    }
    for (cycle in cycles)
    {
      col_names[it] <- paste("accumulated_fatal_od_cycle",cycle,sep="") 
      it <- it+1
    }
    
    counts <- data.frame(counts)
    colnames(counts) <- col_names
    write.csv(counts, file = paste("./output",strategy_id,"/general_stats",run_id,".csv",sep = ""),row.names = block)
}

