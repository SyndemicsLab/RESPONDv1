# Checks the final inputs of the simulation 
check_load_gen_inputs <- function(){
  
  #checking initial cohort vector
  if (anyNA(init_demographics_vec) | ((length(init_demographics_vec) != (ceiling(imax/2)*jmax*kmax*lmax))) | range(init_demographics_vec)[1]<0)
  {
    warning("Invalid values in initial cohort input!")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking entering cohort matrix
  if (anyNA(entering_cohort_matrix) | range(entering_cohort_matrix)[1]<0)
  {
    warning("Invalid values in entering cohort input!")
  }

  if ((ncol(entering_cohort_matrix) != length(time_varying_entering_cohort_cycles)) | (nrow(entering_cohort_matrix) != (length(agegrp)*length(sex))) )
  {
    warning("Invalid number of entering cohort values")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking oud trans matrix
  if (dim(oud_trans_matrix)[1] != total_num_compartments | dim(oud_trans_matrix)[2] != lmax)
  {
    warning("Invalid number of OUD transition values")
  }
  if (length(which(round(rowSums(oud_trans_matrix), digits=6) != 1)) != 0)
  {
    warning("OUD transition values do not add to 1.")
  }
  if (range(oud_trans_matrix)[1] < 0 | range(oud_trans_matrix)[2] > 1)
  {
    warning("Invalid probability values in OUD transition matrix!")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking block transtion matrix
  if ((dim(block_trans_matrix)[1] != total_num_compartments) | (dim(block_trans_matrix)[2] != (length(time_varying_blk_trans_cycles)*(ceiling(imax/2)+1))))
  {
    warning("Invalid number of block transition values")
  }
  for (i in 0:(length(time_varying_blk_trans_cycles)-1))
  {
    if (length(which(round(rowSums(block_trans_matrix[,(i*(num_trts+2)+1):((i+1)*(num_trts+2))]), digits=6) != 1)) != 0)
    {
      warning("Block transition values do not add to 1.")
    }
  }
  if (range(block_trans_matrix)[1] < 0 | range(block_trans_matrix)[2] > 1)
  {
    warning("Invalid probability values in block transition matrix!")
  }
  if (anyNA(block_init_effect_matrix))
  {
    warning("Invalid values in block initiation effect input!")
  }
  if (dim(block_init_effect_matrix)[1] != lmax | dim(block_init_effect_matrix)[2] != imax)
  {
    warning("Invalid number of block initiation effect values")
  }
  if (range(block_init_effect_matrix)[1] < 0 | range(block_init_effect_matrix)[2] > 1)
  {
    warning("Block initiation values should be between 0 and 1.")
  }
  # -----------------------------------------------------------------------------------------------------------------
  # checking overdose inputs
  if (anyNA(all_types_overdose_matrix))
  {
    warning("Invalid values in all types overdose input!")
  }
  if (dim(all_types_overdose_matrix)[1] != (total_num_compartments/2) | dim(all_types_overdose_matrix)[2] != length(time_varying_overdose_cycles))
  {
    warning("Invalid number of all types overdose values")
  }
  if (range(all_types_overdose_matrix)[1] < 0 | range(all_types_overdose_matrix)[2] > 1)
  {
    warning("All type overdose values should be between 0 and 1.")
  }
  
  if (anyNA(fatal_overdose_vec))
  {
    warning("Invalid values in fatal overdose input!")
  }
  if (range(fatal_overdose_vec)[1] < 0 | range(fatal_overdose_vec)[2] > 1)
  {
    warning("Fatal to all-type overdose values should be between 0 and 1.")
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
  if (range(mort_vec)[1] < 0 | range(mort_vec)[2] > 1)
  {
    warning("Mortality values should be between 0 and 1.")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # Checking healthcare system cost
  # healthcare utilization and overdose cost
  if (cost_analysis == "yes")
  {
    if (anyNA(healthcare_utilization_cost) | range(healthcare_utilization_cost)[1] < 0)
    {
      warning("Invalid values in healthcare utilization cost inputs!")
    } 
    if (nrow(healthcare_utilization_cost) != (total_num_compartments) | (ncol(healthcare_utilization_cost) != (length(cost_perspectives))))
    {
      warning("Invalid number of healthcare utilization costs")
    }
    if (anyNA(overdose_cost)| range(overdose_cost)[1] < 0)
    {
      warning("Invalid values in overdose cost inputs!")
    } 
    if (nrow(overdose_cost) != 2 | (ncol(overdose_cost) != (length(cost_perspectives))))
    {
      warning("Invalid number of overdose costs")
    }
    if (num_trts != 0)
    {
      if (anyNA(treatment_utilization_cost) | anyNA(pharmaceutical_cost))
      {
        warning("Invalid values in treatment utilization or pharmaceutical cost!")
      }
      
      if (range(treatment_utilization_cost)[1]<0 | range(pharmaceutical_cost)[1]<0)
      {
        warning("Invalid values in treatment utilization or pharmaceutical cost!")
      }
      
      if (nrow(treatment_utilization_cost) != num_trts | (ncol(treatment_utilization_cost) != (length(cost_perspectives))))
      {
        warning("Invalid number of treatment utilization costs")
      }
      if (nrow(pharmaceutical_cost) != num_trts | (ncol(pharmaceutical_cost) != (length(cost_perspectives))))
      {
        warning("Invalid number of pharmaceutical costs")
      }
    }
    
    # Utility
    if (ncol(util) != 2 | nrow(util) != total_num_compartments)
    {
      warning("Invalid number of utility values")
    }
    if (min(util) < 0 | max(util) > 1)
    {
      warning("Utilities must be between 0 and 1!")
    }
  }
}
