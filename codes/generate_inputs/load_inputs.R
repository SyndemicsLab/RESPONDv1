### read in required .csv data files and prepare them for simulation ###

load_inputs <- function() {

  #cohort initialization inputs
  init_demographics_vec <<- read.csv("inputs/init_cohort.csv")$counts
  
  #------------------------------------------------------------------------------------------------------------
  # entering cohort inputs
  entering_cohort_matrix <<- as.matrix(read.csv("inputs/entering_cohort.csv"))

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

  fatal_overdose_matrix <<- as.matrix(read.csv("inputs/fatal_overdose.csv"))

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
}


