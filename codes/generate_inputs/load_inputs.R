### read in required .csv data files and prepare them for simulation ###

load_inputs <- function() {

  #cohort initialization inputs
  init_demographics_vec <<- as.matrix(read.csv(initial_cohort_file)$counts)
  
  #------------------------------------------------------------------------------------------------------------
  # entering cohort inputs
  tmp_csv <- read.csv(entering_cohort_file,comment.char="",check.names = FALSE)
  if (ncol(tmp_csv) == 3)
  {
    col_name <- colnames(tmp_csv[3])
  }
  entering_cohort_matrix <<- as.matrix(tmp_csv[,3:ncol(tmp_csv)])
  
  if(ncol(entering_cohort_matrix) != length(time_varying_entering_cohort_cycles))
  {
    warning("Number of time-varying entering cohort columns in input table should be the same as length of time_varying_entering_cohort_cycles in user_input.R ")
  }
  
  if (ncol(entering_cohort_matrix) == 1)
  {
    pos = gregexpr('_cycle',col_name)
    str <- substr(col_name, pos[[1]][1]+6, nchar(col_name))
    if (str != time_varying_entering_cohort_cycles[1])
    {
      warning("Enetering cohort cycles in .csv file should be the same as cycles in user_inputs.R")
    }
  } else {
    for (i in 1:length(time_varying_entering_cohort_cycles))
    {
      pos = gregexpr('_cycle',  colnames(entering_cohort_matrix)[i])
      str <- substr(colnames(entering_cohort_matrix)[i], pos[[1]][1]+6, nchar(colnames(entering_cohort_matrix)[i]))
      if (str != time_varying_entering_cohort_cycles[i])
      {
        warning("Enetering cohort cycles in .csv file should be the same as cycles in user_inputs.R")
      }
    }
  }
  #------------------------------------------------------------------------------------------------------------
  #OUD transition inputs
  tmp_csv <- read.csv(oud_trans_file)
  oud_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  # # -----------------------------------------------------------------------------------------------------------
  # # Block transition inputs
  tmp_csv <- read.csv(block_trans_file,comment.char="",check.names = FALSE)
  block_trans_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  tmp <- tmp_csv[which(tmp_csv$initial_block==block[1]),]
  post_trt_cols <- grep("post_trt",colnames(tmp))
  tmp <- tmp[,post_trt_cols]
  if (length(which(tmp != 0) != 0))
  {
    warning("There is no post-treatment for no-treatment block, so the corresponding block transition value should be 0!")
  }
  
  tmp_csv <- read.csv(block_init_effect_file)
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
  tmp_csv <- read.csv(all_type_overdose_file,comment.char="",check.names = FALSE)
  if (ncol(tmp_csv) == 5)
  {
    col_name_all <- colnames(tmp_csv[5])
  }
  all_types_overdose_matrix <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])

  fatal_overdose_vec <<- as.matrix(read.csv(fatal_overdose_file,comment.char="",check.names = FALSE))
  if (ncol(fatal_overdose_vec) == 1)
  {
    col_name_f <- colnames(fatal_overdose_vec)
  }
  
  if((ncol(all_types_overdose_matrix) != length(time_varying_overdose_cycles)) | (ncol(fatal_overdose_vec) != length(time_varying_overdose_cycles)))
  {
    warning("Number of time-varying overdose columns in input table should be the same as length of time_varying_overdose_cycles in user_input.R ")
  }

  if (ncol(all_types_overdose_matrix) == 1)
  {
    pos1 = gregexpr('_cycle',col_name_all)
    pos2 = gregexpr('_cycle',col_name_f)
    str1 <- substr(col_name_all, pos1[[1]][1]+6, nchar(col_name_all))
    str2 <- substr(col_name_f, pos2[[1]][1]+6, nchar(col_name_f))
    if (! (str1 == str2 & (str1 == time_varying_overdose_cycles[1])) )
    {
      warning("All types and fatal overdose cycles in .csv file should be the same as cycles in user_inputs.R")
    }
  } else {
    for (i in 1:length(time_varying_overdose_cycles))
    {
      pos1 = gregexpr('_cycle',  colnames(all_types_overdose_matrix)[i])
      pos2 = gregexpr('_cycle',  colnames(fatal_overdose_vec)[i])
      str1 <- substr(colnames(all_types_overdose_matrix)[i], pos1[[1]][1]+6, nchar(colnames(all_types_overdose_matrix)[i]))
      str2 <- substr(colnames(fatal_overdose_vec)[i], pos2[[1]][1]+6, nchar(colnames(fatal_overdose_vec)[i]))   
      if (! (str1 == str2 & (str1 == time_varying_overdose_cycles[i])) )
      {
        warning("All types and fatal overdose cycles in .csv file should be the same as cycles in user_inputs.R")
      }
    }
  }

  # -----------------------------------------------------------------------------------------------------------
  # background Mortality inputs
  bg_mort <- as.matrix(read.csv(background_mortality_file)$death_prob)
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
  
  SMR <- as.matrix(read.csv(SMR_file)$SMR)
  if (anyNA(SMR))
  {
    warning("Invalid values in SMR input!")
  } 
  if (length(SMR) != total_num_compartments)
  {
    warning("Invalid number of SMR values")
  }
  if (range(SMR)[1]<1)
  {
    warning("SMR values cannot be less than 1!")
  }
  bg_mort <- rep(rep(bg_mort,each= lmax),imax)
  mort_vec <<- 1-exp(log(1-bg_mort)*SMR)
  
  # -----------------------------------------------------------------------------------------------------------
  # Costs and Utility
  if (cost_analysis == "yes")
  {
    tmp_csv <- read.csv(healthcare_utilization_cost_file)
    healthcare_utilization_cost <<- as.matrix(tmp_csv[,5:ncol(tmp_csv)])
    
    tmp_csv <- read.csv(overdose_cost_file)
    overdose_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
    
    # treatment utilization and pharmaceutical cost
    if (num_trts == 0)
    {
      treatment_utilization_cost <<- t(rep(0,length(cost_perspectives)))
      pharmaceutical_cost <<- t(rep(0,length(cost_perspectives)))
    } else {
      tmp_csv <- read.csv(treatment_utilization_cost_file)
      treatment_utilization_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
      
      tmp_csv <- read.csv(pharmaceutical_cost_file)
      pharmaceutical_cost <<- as.matrix(tmp_csv[,2:ncol(tmp_csv)])
    }
    
    bg_util <- read.csv(background_utility_file)$utility
    oud_util <- read.csv(oud_utility_file)$utility
    setting_util <- read.csv(setting_utility_file)$utility
    if (length(bg_util) != jmax*kmax | min(bg_util) < 0 | max(bg_util) > 1)
    {
      warning("Invalid number of background utility values")
    }
    if (length(oud_util) != imax*lmax | min(oud_util) < 0 | max(oud_util) > 1)
    {
      warning("Invalid number of OUD utility values")
    }
    if (length(setting_util) != imax | min(setting_util) < 0 | max(setting_util) > 1)
    {
      warning("Invalid number of setting utility values")
    }
    bg_util <- rep(rep(bg_util, each=lmax),imax)
    oud_util_seq <- seq(1,length(oud_util),lmax)
    tmp2 <- c(-1)
    for (i in 1:length(oud_util_seq))
    {
      tmp <- oud_util[oud_util_seq[i]:(i*lmax)]
      tmp2 <- c(tmp2,rep(tmp,jmax*kmax))
    }
    oud_util <- tmp2[-1]
    setting_util <- rep(setting_util,each=total_num_compartments/imax)
    util <- cbind(bg_util,oud_util,setting_util)
    util_min <- apply(util,1,min)
    util_mult <- apply(util,1, prod)
    util <<- cbind(util_min,util_mult)
    
    #factor_perm<-expand.grid(oud,sex,agegrp,block) 
    #colnames(factor_perm)<-c("oud","sex","agegrp","block")
    #factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
    #df <- data.frame(factor_perm,bg_util,oud_util,setting_util, util_min, util_mult)
    #write.csv(df,"util.csv")
    
  } 
  
}


