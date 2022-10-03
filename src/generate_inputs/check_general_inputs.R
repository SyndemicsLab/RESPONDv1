# Checks some common errors in genral user input file and automatically creates other variables based on them
check_general_inputs <- function() {
  # Checking some general inputs
  if (simulation_duration %% periods != 0)
  {
    warning("Unacceptable duration/periods")
  }
  
  if (((length(block)%%2) == 0) | (length(unique(block)) != length(block)))
    warning("unacceptable blocks")

# check if order of post treatments is the same as order of treatments
  tmp <- 0
  num_trts <<- (length(block)-1)/2
  if (num_trts > 0)
  {
    for (i in 2:(num_trts+1))
    {
      if (tolower(block[i+num_trts]) != tolower(paste("post",block[i],sep = "-")))
      {
        tmp <- tmp+1
      }
    }
  }
  if (tmp != 0)
    warning("unacceptable blocks")

  if (length(unique(agegrp)) != length(agegrp))
    warning("unacceptable age brackets")

  if (((length(oud)%%2) != 0) | (length(unique(oud)) != length(oud)))
    warning("unacceptable oud states")

  # creating some new variables
  # Here, i, j, k & l represent treatment episode, age bracket, gender and OUD status respectively.
  imax <<- length(block)                # maximum number of blocks (no_treatment + treatments + post_treatments)
  jmax <<- length(agegrp)               # maximum number of age brackets
  kmax <<- length(sex)                  # maximum number of genders
  lmax <<- length(oud)                  # maximum number of OUD states
  total_num_compartments <<- imax*jmax*kmax*lmax      # total number of compartments

  # For detox intervention
  detox_block_id <<- -1                 # block ID of detox center (IDs start from 0)
  tmp2 <- which(block=="detox")          # if there is no detox in the block vector, assign the ID as -1
  if (length(tmp2) != 0)
    detox_block_id <<- tmp2-1               # -1 for index adjustment
  
  # check entering cohort cycles
  if (time_varying_entering_cohort_cycles[length(time_varying_entering_cohort_cycles)] <  simulation_duration)
  {
    warning("The last cycle in entering cohort cycles should be greater than or equal to simulation duration.")
  }
  if (length(time_varying_entering_cohort_cycles) != length(unique(time_varying_entering_cohort_cycles)))
  {
    warning("Time-varying entering cohort cycles should be identical.")
  }
  if (!isTRUE(all.equal(sort(time_varying_entering_cohort_cycles),time_varying_entering_cohort_cycles)))
  {
    warning("Time-varying entering cohort cycles should be in increasing order")
  }
  # ----------------------------------------------------------------------------------------------------------------
  # check overdose cycles
  if (time_varying_overdose_cycles[length(time_varying_overdose_cycles)] < simulation_duration)
  {
    warning("The last cycle in overdose cycles should be greater than or equal to simulation duration.")
  }
  if (length(time_varying_overdose_cycles) != length(unique(time_varying_overdose_cycles)))
  {
    warning("Time-varying overdose cycles should be identical.")
  }
  if(!isTRUE(all.equal(sort(time_varying_overdose_cycles),time_varying_overdose_cycles)))
  {
    warning("Time-varying overdose cycles should be in increasing order")
  }
  # -----------------------------------------------------------------------------------------------------------------
  # check block transition cycles
  if (time_varying_blk_trans_cycles[length(time_varying_blk_trans_cycles)] < simulation_duration)
  {
    warning("The last cycle in block transition cycles should be greater than or equal to simulation duration.")
  }
  if (length(time_varying_blk_trans_cycles) != length(unique(time_varying_blk_trans_cycles)))
  {
    warning("Time-varying block transition cycles should be identical.")
  }
  if(!isTRUE(all.equal(sort(time_varying_blk_trans_cycles),time_varying_blk_trans_cycles)))
  {
    warning("Time-varying block transition cycles should be in increasing order")
  }
  # -----------------------------------------------------------------------------------------------------------------
  # check general stats cycles
  tmp <- sort(general_stats_cycles)
  if(!isTRUE(all.equal(tmp,general_stats_cycles)))
  {
    warning("general stat cycles should be in increasing order!")
  }

  if (cost_analysis == "no")
  {
    healthcare_utilization_cost <<- matrix(c(-1,-1),nrow = 1)
    treatment_utilization_cost <<- matrix(c(-1,-1),nrow = 1)
    pharmaceutical_cost <<- matrix(c(-1,-1),nrow = 1)
    overdose_cost <<- matrix(c(-1,-1),nrow = 1)
    util <<- matrix(c(-1,-1),nrow = 1)
  }
}
