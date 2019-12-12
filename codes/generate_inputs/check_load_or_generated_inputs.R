# Checks the final inputs of the simulation 
check_load_gen_inputs <- function(){
  
  #checking initial cohort vector
  if (anyNA(init_demographics_vec) | ((length(init_demographics_vec) != (ceiling(imax/2)*jmax*kmax*lmax))))
  {
    warning("Invalid values in initial cohort input!")
  }
  # ------------------------------------------------------------------------------------------------------------------
  # checking entering cohort matrix
  if (anyNA(entering_cohort_matrix))
  {
    warning("Invalid values in entering cohort input!")
  }

  if ((ncol(entering_cohort_matrix) != length(time_varying_entering_cohort_cycles)) | (nrow(entering_cohort_matrix) != (length(agegrp)*length(sex))) )
  {
    warning("Invalid number of entering cohort values")
  }
  if (!isTRUE(all.equal(colSums(entering_cohort_matrix),entering_cohort_total_size, check.attributes = FALSE, tolerance = 0.00001)))
  {
    warning("Values of each column in entering_cohort_matrix should add to total N")
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
  if (range(block_init_effect_matrix)[1] < 0 | range(block_init_effect_matrix)[2] > 1)
  {
    warning("Invalid probability values in block initiation effect matrix!")
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
  if (range(all_types_overdose_matrix)[1] < 0 | range(all_types_overdose_matrix)[2] > 1)
  {
    warning("Invalid probability values in all types overdose matrix!")
  }
  
  if (anyNA(fatal_overdose_vec))
  {
    warning("Invalid values in fatal overdose input!")
  }
  if (range(fatal_overdose_vec)[1] < 0 | range(fatal_overdose_vec)[2] > 1)
  {
    warning("Fatal to all-type overdose values should be between 0 and 1.")
  }
  if (range(fatal_overdose_vec)[1] < 0 | range(fatal_overdose_vec)[2] > 1)
  {
    warning("Invalid probability values in fatal overdose vector!")
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
  }# ------------------------------------------------------------------------------------------------------------------
  # Checking healcare system cost
  # healthcare utilization and overdose cost
  if (cost_analysis == "yes")
  {
    if (anyNA(healthcare_utilization_cost))
    {
      warning("Invalid values in healthcare utilization cost inputs!")
    } 
    if (nrow(healthcare_utilization_cost) != (total_num_compartments/imax) | (ncol(healthcare_utilization_cost) != (length(cost_perspectives))))
    {
      warning("Invalid number of healthcare utilization costs")
    }
    if (anyNA(overdose_cost))
    {
      warning("Invalid values in overdose cost inputs!")
    } 
    if (nrow(overdose_cost) != 2 | (ncol(overdose_cost) != (length(cost_perspectives))))
    {
      warning("Invalid number of overdose costs")
    }
    # if (anyNA(utility))
    # {
    #   warning("Invalid values in utility inputs!")
    # } 
    # if (nrow(utility) != total_num_compartments & ncol(utility) != 2)
    # {
    #   warning("Invalid number of utility values")
    # }
    # treatment utilizatiohn and pharmaceutical cost
    if (num_trts != 0)
    {
      if (anyNA(treatment_utilization_cost) | anyNA(pharmaceutical_cost))
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
  }
}