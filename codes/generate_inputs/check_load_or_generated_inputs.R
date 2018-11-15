# Checks the final inputs of the simulation 
check_load_gen_inputs <- function(){
  
  #checking initial cohort vector
  if (anyNA(init_demographics_vec) || (length(init_demographics_vec) != (ceiling(imax/2)*jmax*kmax*lmax)))
  {
    warning("Invalid values in initial cohort input!")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking entering cohort matrix
  if (anyNA(entering_cohort_matrix))
  {
    warning("Invalid values in entering cohort input!")
  }
  if (isFALSE(all.equal(entering_cohort_matrix[,1],time_varying_entering_cohort_cycles,check.attributes = FALSE)))
  {
    warning("Entering cohort cycles in .csv file should be the same as cycles in user_inputs.R")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking oud trans matrix
  if (dim(oud_trans_matrix)[1] != total_num_compartments || dim(oud_trans_matrix)[2] != lmax)
  {
    warning("Invalid number of OUD transition values")
  }
  if (length(which(round(rowSums(oud_trans_matrix), digits=12) != 1)) != 0)
  {
    warning("OUD transition values do not add to 1.")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking block transtion matrix
  if (dim(block_trans_matrix)[1] != total_num_compartments || dim(block_trans_matrix)[2] != (ceiling(imax/2)+1))
  {
    warning("Invalid number of block transition values")
  }
  if (length(which(round(rowSums(block_trans_matrix), digits=12) != 1)) != 0)
  {
    warning("Block transition values do not add to 1.")
  }
  if (anyNA(block_init_effect_matrix))
  {
    warning("Invalid values in block initiation effect input!")
  }
  if (dim(block_init_effect_matrix)[1] != lmax || dim(block_init_effect_matrix)[2] != imax)
  {
    warning("Invalid number of block initiation effect values")
  }
  # -----------------------------------------------------------------------------------------------------------------
  # checking overdose inputs
  if (anyNA(all_types_overdose_matrix))
  {
    warning("Invalid values in all types overdose input!")
  }
  if (dim(all_types_overdose_matrix)[1] != (total_num_compartments/2) || dim(all_types_overdose_matrix)[2] != length(time_varying_overdose_cycles))
  {
    warning("Invalid number of all types overdose values")
  }
  
  if (anyNA(fatal_overdose_matrix))
  {
    warning("Invalid values in fatal overdose input!")
  } 
  if (isFALSE(all.equal(fatal_overdose_matrix[,1],time_varying_overdose_cycles,check.attributes = FALSE)))
  {
    warning("Fatal overdose cycles in .csv file should be the same as cycles in user_inputs.R")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking mortality inputs
  if (anyNA(mort_vec))
  {
    warning("Invalid values in mortality input!")
  } 
  if (length(mort_vec) != total_num_compartments)
  {
    warning("Invalid number of mortality values")
  }
  # ------------------------------------------------------------------------------------------------------------------
}