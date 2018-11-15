get_general_stats_in_cycle <- function (cycles)
{
  block_size <- jmax*kmax*lmax
  counts <- matrix(nrow = imax, ncol = length(cycles)*(2+lmax))   # 1 for total size, 1 for male count and lmax for all oud states
  
  for (i in 1:imax) # for each block
  {
    it <- 1
    # total size
    for (cycle in cycles)   
    {
      counts[i,it] <- sum(out$`general outputs`[cycle+1,which(substring(general_IDs, 2, 3) == block_idx[i])])
      it <- it+1
    }
    
    # male counts
    for (cycle in cycles)   
    {
      counts[i,it] <- sum(out$`general outputs`[cycle+1,(which(substring(general_IDs, 2, 3) == block_idx[i] & substring(general_IDs,6,7) == sex_idx[1]))])
      it <- it+1
    }
    
    # oud counts
    for (l in 1:lmax)
    {
      for (cycle in cycles)   
      {
        counts[i,it] <- sum(out$`general outputs`[cycle+1,(which(substring(general_IDs, 2, 3) == block_idx[i] & substring(general_IDs,8,9) == oud_idx[l]))])
        it <- it+1
      }
    }
  }
    it <- 1
    col_names <- rep("",length(cycles)*(2+lmax))
    for (cycle in cycles)
    {
      col_names[it] <- paste("total_size_c",cycle,sep="") 
      it <- it+1
    }
    for (cycle in cycles)
    {
      col_names[it] <- paste("male_counts_c",cycle,sep="") 
      it <- it+1
    }
    for (l in 1:lmax)
    {
      for (cycle in cycles)
      {
        col_names[it] <- paste(oud[l],"_c",cycle,sep="") 
        it <- it+1
      }
    }

    write.table(counts, file = paste("outputs/general_stats",run_id,".csv",sep = ""),sep = ",", col.names = col_names,row.names = FALSE)
    
}

