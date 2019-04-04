### read in required .csv data files and prepare them for simulation ###

load_inputs <- function() {

  #cohort initialization inputs
  init_demographics_vec <<- read.csv("inputs/init_cohort.csv")$counts
  
  #------------------------------------------------------------------------------------------------------------
  # entering cohort inputs
  
  tmp_csv <- read.csv("inputs/entering_cohort.csv")
  tmp_csv <- as.matrix(tmp_csv[,3:ncol(tmp_csv)])
  entering_cohort_matrix <<- tmp_csv
  for (i in 1:length(entering_cohort_total_size))
  {
    entering_cohort_matrix[,i] <<- tmp_csv[,i]*entering_cohort_total_size[i]
  }

  #------------------------------------------------------------------------------------------------------------
  #OUd transition inputs
  tmp_csv <- read.csv("inputs/oud_trans.csv")
  oud_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  # # -----------------------------------------------------------------------------------------------------------
  # # Block transition inputs
  tmp_csv <- read.csv("inputs/block_trans.csv")
  block_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])
  
  tmp_csv <- read.csv("inputs/block_init_effect.csv")
  block_init_effect_matrix <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
  
  # # -----------------------------------------------------------------------------------------------------------
  # # Overdose inputs
  tmp_csv <- read.csv("inputs/all_types_overdose.csv")
  all_types_overdose_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  fatal_overdose_vec <<- as.numeric(read.csv("inputs/fatal_overdose.csv"))

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


