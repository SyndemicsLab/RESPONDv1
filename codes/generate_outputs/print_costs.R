print_costs <- function()
{
  total_cost <- matrix(nrow = simulation_duration/periods, ncol = imax*length(cost_perspectives))
  for(c in 1:(simulation_duration/periods))
  {
    for (ii in 1:length(cost_perspectives))
    {
      for (i in 1:imax)
      {
        if (i > 1 & i <= (num_trts+1))  #actual treatment episodes
        {
          total_cost[c,((ii-1)*imax+i)] <- (out$healthcare_utilization_cost[c,((ii-1)*imax+i)] + out$treatment_utilization_cost[c,((ii-1)*num_trts+i-1)]
                              + out$pharmaceutical_cost[c,((ii-1)*num_trts+i-1)] + out$overdose_cost[c,((ii-1)*imax+i)])
        } else {
          total_cost[c,((ii-1)*imax+i)] <- out$healthcare_utilization_cost[c,((ii-1)*imax+i)] + out$overdose_cost[c,((ii-1)*imax+i)]
        }
      }
    }
  }
  
  it <- 1
  col_names <- rep("",imax*length(cost_perspectives))
  for (j in 1:length(cost_perspectives))
  {
    for (i in 1:imax)
    {
      col_names[it] <- paste(block[i],cost_perspectives[j],sep = "_")
      it <- it+1
    }
  }
  df <- data.frame(total_cost)
  colnames(df) <- col_names
  write.csv(df,file=paste("./output",strategy_id,"/cost_life/total_costs",run_id,".csv",sep = ""), quote= FALSE)

  df <- data.frame(out$total_cost_per_perspective)
  colnames(df) <- c("total_cost","total_discounted_cost")
  write.csv(df,file=paste("./output",strategy_id,"/cost_life/CE_costs",run_id,".csv",sep=""), row.names = cost_perspectives,quote= FALSE)
 #-----------------------------------------------------------------------------------------------------------------
  # print cost categories
  if (print_cost_categories == "yes")
  {
    #print healthcare utilization cost
    per_perspective_costs <- dim(out$healthcare_utilization_cost)[2]/length(cost_perspectives)
    it <- 1
    col_names <- rep("",dim(out$healthcare_utilization_cost)[2])
    for (j in 1:length(cost_perspectives))
    {
      for (i in 1:per_perspective_costs)
      {
        col_names[it] <- paste(block[i],cost_perspectives[j],sep = "_")
        it <- it+1
      }
    }
    df <- data.frame(out$healthcare_utilization_cost)
    colnames(df) <- col_names
    write.csv(df,file=paste("./output",strategy_id,"/cost_life/healthcare_utilization_cost",run_id,".csv",sep = ""), quote= FALSE)
    df <- data.frame(out$overdose_cost)
    colnames(df) <- col_names
    write.csv(df,file=paste("./output",strategy_id,"/cost_life/overdose_cost",run_id,".csv", sep=""), quote= FALSE)
    
    # print treatment utilization and pharmaceutical cost
    per_perspective_costs <- dim(out$treatment_utilization_cost)[2]/length(cost_perspectives)
    it <- 1
    col_names <- rep("",dim(out$treatment_utilization_cost)[2])
    for (j in 1:length(cost_perspectives))
    {
      for (i in 1:per_perspective_costs)
      {
        col_names[it] <- paste(block[i+1],cost_perspectives[j],sep = "_")
        it <- it+1
      }
    }
    df <- data.frame(out$treatment_utilization_cost)
    colnames(df) <- col_names
    write.csv(df,file=paste("./output",strategy_id,"/cost_life/treatment_utilization_cost",run_id,".csv",sep=""), quote= FALSE)
    df <- data.frame(out$pharmaceutical_cost)
    colnames(df) <- col_names
    write.csv(df,file=paste("./output",strategy_id,"/cost_life/pharmaceutical_cost",run_id,".csv",sep=""), quote= FALSE)
  }
  
}
  