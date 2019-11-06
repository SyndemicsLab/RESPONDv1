### read in required .csv data files and prepare them for simulation ###

load_inputs <- function() {

  #cohort initialization inputs
  init_demographics_vec <<- read.csv("inputs/init_cohort.csv")$counts
  
  #------------------------------------------------------------------------------------------------------------
  # entering cohort inputs
  
  tmp_csv <- read.csv("inputs/entering_cohort.csv",comment.char="",check.names = FALSE)
  tmp_csv <- as.matrix(tmp_csv[,3:ncol(tmp_csv)])
  entering_cohort_matrix <<- tmp_csv
  for (i in 1:length(entering_cohort_total_size))
  {
    entering_cohort_matrix[,i] <<- tmp_csv[,i]*entering_cohort_total_size[i]
  }
  
  if(ncol(entering_cohort_matrix) != length(time_varying_entering_cohort_cycles))
  {
    warning("Number of time-varying entering cohort columns in input table should be the same as length of time_varying_entering_cohort_cycles in user_input.R ")
  }
  
  for (i in 1:length(time_varying_entering_cohort_cycles))
  {
    pos = gregexpr('_cycle',  colnames(entering_cohort_matrix)[i])
    str <- substr(colnames(entering_cohort_matrix)[i], pos[[1]][1]+6, nchar(colnames(entering_cohort_matrix)[i]))
    if (str != time_varying_entering_cohort_cycles[i])
    {
      warning("Enetering cohort cycles in .csv file should be the same as cycles in user_inputs.R")
    }
  }

  #------------------------------------------------------------------------------------------------------------
  #OUD transition inputs
  tmp_csv <- read.csv("inputs/oud_trans.csv")
  oud_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  # # -----------------------------------------------------------------------------------------------------------
  # # Block transition inputs
  tmp_csv <- read.csv("inputs/block_trans.csv",comment.char="",check.names = FALSE)
  block_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])
  
  tmp_csv <- read.csv("inputs/block_init_effect.csv")
  block_init_effect_matrix <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
  
  if(ncol(block_trans_matrix) != (length(time_varying_blk_trans_cycles)*(ceiling(imax/2)+1)) )
  {
    warning("Number of time-varying block transition columns in input table should correspond to length of time_varying_blk_trans_cycles in user_input.R ")
  }
  
  per_time_interval_col <- ceiling(imax/2)+1
  for (i in 1:length(time_varying_blk_trans_cycles))
  {
    for (ii in 1:per_time_interval_col)
    {
      pos = gregexpr('_cycle',  colnames(block_trans_matrix)[(i-1)*per_time_interval_col+ii])
      str <- substr(colnames(block_trans_matrix)[(i-1)*per_time_interval_col+ii], pos[[1]][1]+6, nchar(colnames(block_trans_matrix)[(i-1)*per_time_interval_col+ii]))
      if (str != time_varying_blk_trans_cycles[i])
      {
        warning("Block transition cycles in .csv file should be the same as cycles in user_inputs.R")
      }
    }
  }
  # # -----------------------------------------------------------------------------------------------------------
  # # Overdose inputs
  tmp_csv <- read.csv("inputs/all_types_overdose.csv",comment.char="",check.names = FALSE)
  all_types_overdose_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  fatal_overdose_vec_tmp <- read.csv("inputs/fatal_overdose.csv",comment.char="",check.names = FALSE)
  fatal_overdose_vec <<- as.numeric(fatal_overdose_vec_tmp)
  
  if((ncol(all_types_overdose_matrix) != length(time_varying_overdose_cycles)) | (ncol(fatal_overdose_vec_tmp) != length(time_varying_overdose_cycles)))
  {
    warning("Number of time-varying overdose columns in input table should be the same as length of time_varying_overdose_cycles in user_input.R ")
  }
  
  for (i in 1:length(time_varying_overdose_cycles))
  {
     pos1 = gregexpr('_cycle',  colnames(all_types_overdose_matrix)[i])
     pos2 = gregexpr('_cycle',  colnames(fatal_overdose_vec_tmp)[i])
     str1 <- substr(colnames(all_types_overdose_matrix)[i], pos1[[1]][1]+6, nchar(colnames(all_types_overdose_matrix)[i]))
     str2 <- substr(colnames(fatal_overdose_vec_tmp)[i], pos2[[1]][1]+6, nchar(colnames(fatal_overdose_vec_tmp)[i]))   
     if (! (str1 == str2 & (str1 == time_varying_overdose_cycles[i])) )
     {
       warning("All types and fatal overdose cycles in .csv file should be the same as cycles in user_inputs.R")
     }
  }
  # -----------------------------------------------------------------------------------------------------------
  # background Mortality inputs
  bg_mort <- read.csv("inputs/background_mortality.csv")$death_prob
  if (anyNA(bg_mort))
  {
    warning("Invalid values in background input!")
  } 
  if (length(bg_mort) != jmax*kmax)
  {
    warning("Invalid number of background mortality values")
  }
  if (range(bg_mort)[1] < 0 | range(bg_mort)[2] >= 1)
  {
    warning("Background mortality values should be between 0 and 1(exclusive).")
  }
  
  SMR <- read.csv("inputs/SMR.csv")$SMR
  if (anyNA(SMR))
  {
    warning("Invalid values in SMR input!")
  } 
  if (length(SMR) != total_num_compartments)
  {
    warning("Invalid number of SMR values")
  }
  bg_mort <- rep(rep(bg_mort,each= lmax),imax)
  mort_vec <<- 1-exp(log(1-bg_mort)*SMR)
  
  # -----------------------------------------------------------------------------------------------------------
  # Costs and quality of life
  if (cost_analysis == "yes")
  {
    tmp_csv <- read.csv("inputs/cost_life/healthcare_utilization_cost.csv")
    healthcare_utilization_cost <<- as.matrix(tmp_csv[,4:ncol(tmp_csv)])
    
    tmp_csv <- read.csv("inputs/cost_life/overdose_cost.csv")
    overdose_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
    
    #tmp_csv <- read.csv("inputs/cost_life/utility.csv")
    #utility <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])
    
    # treatment utilization and pharmaceutical cost
    if (num_trts != 0)
    {
      tmp_csv <- read.csv("inputs/cost_life/treatment_utilization_cost.csv")
      treatment_utilization_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
      
      tmp_csv <- read.csv("inputs/cost_life/pharmaceutical_cost.csv")
      pharmaceutical_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
    }
  } 
  
}


