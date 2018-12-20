# Checks some common errors in genral user input file and automatically creates other variables based on them
check_general_inputs <- function() {
  # Checking some general inputs
  if (((length(block)%%2) == 0) | (length(unique(block)) != length(block)))
    warning("unacceptable blocks")

# check if order of post treatments is the same as order of treatments
  tmp <- 0
  num_trts <- (length(block)-1)/2
  if (num_trts > 0)
  {
    for (i in 2:(num_trts+1))
    {
      if (block[i+num_trts] != paste("post",block[i],sep = "_"))
      {
        tmp <- tmp+1
      }
    }
  }
  if (tmp != 0)
    warning("unacceptable blocks")

  if (length(unique(agegrp)) != length(agegrp))
    stop("unacceptable age brackets")

  if (((length(oud)%%2) != 0) | (length(unique(oud)) != length(oud)))
    warning("unacceptable oud states")

  # creating some new variables
  # Here, i, j, k & l represent treatment episode, age bracket, gender and OUD status respectively.
  imax <<- length(block)                # maximum number of blocks (no_treatment + treatments + post_treatments)
  jmax <<- length(agegrp)               # maximum number of age brackets
  kmax <<- length(sex)                  # maximum number of genders
  lmax <<- length(oud)                  # maximum number of OUD states
  total_num_compartments <<- imax*jmax*kmax*lmax      # total number of compartments

  detox_block_id <<- -1                 # block ID of detox center (IDs start from 0)
  tmp2 <- which(block=="detox")          # if there is no detox in the block vector, assign the ID as -1
  if (length(tmp2) != 0)
    detox_block_id <<- tmp2-1               # -1 for index adjustment

  # check if the last cycle for time_varying parameters is equal or greater than simulation duration
  if (time_varying_entering_cohort_cycles[length(time_varying_entering_cohort_cycles)] <  simulation_duration)
  {
    warning("The last cycle in entering cohort table should be greater than or equal to simulation duration.")
  }
  if (time_varying_overdose_cycles[length(time_varying_overdose_cycles)] < simulation_duration)
  {
    warning("The last cycle in overdose table should be greater than or equal to simulation duration.")
  }
}
