# Call the functions that generate stochastic inputs.
# Final results will be directly passed to simulation model. They can also be saved on disk as .csv files (optional)
# initial cohort and background mortality are deterministic, so they are generated within this function.

generate_inputs <- function (){
  source("codes/generate_inputs/generate_stochastic_input_functions.R")
  
  # entering cohort
  entering_cohort_matrix <<- generate_entering_cohort_size()
  if (save_input_files_as_csv == "yes")
  {
    factor_perm<-expand.grid(sex,agegrp)
    colnames(factor_perm)<-c("sex","agegrp")
    factor_perm<-factor_perm[,c("agegrp","sex")]
    col_names <- c(paste("number_of_new_comers_c",time_varying_entering_cohort_cycles[1],sep=""))
    for (i in 2:(length(time_varying_entering_cohort_cycles)))
    {
      col_names <- c(col_names,paste("number_of_new_comers_c",time_varying_entering_cohort_cycles[i],sep=""))
    }
    entering_cohort_tbl <- data.frame(factor_perm,entering_cohort_matrix)
    colnames(entering_cohort_tbl) <- c("agegrp","sex",col_names)
    write.csv(entering_cohort_tbl,file=paste("inputs/entering_cohort",run_id,".csv", sep=''),row.names = FALSE,quote = FALSE)
  }
  # -----------------------------------------------------------------------------------------------------------------
  # mortality
  bg_mort <- read.csv("inputs/background_mortality.csv")$death_prob
  if (anyNA(bg_mort))
  {
    warning("Invalid values in background input!")
  } 
  if (length(bg_mort) != jmax*kmax)
  {
    warning("Invalid number of background mortality values")
  }
  bg_mort <- rep(rep(bg_mort,each= lmax),imax)
  SMR <- generate_stochastic_SMR()
  mort_vec <<- 1-exp(log(1-bg_mort)*SMR)

  if (save_input_files_as_csv == "yes")
  {
    factor_perm<-expand.grid(oud,sex,agegrp,block)
    colnames(factor_perm)<-c("oud","sex","agegrp","block")
    factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
    mortality_prob <- data.frame(factor_perm,bg_mort,SMR,mort_vec)
    colnames(mortality_prob) <- c("block","agegrp","sex","oud","background_mortality","SMR","mortality_prob")
    write.csv(mortality_prob,file=paste("inputs/mortality",run_id,".csv", sep=''),row.names = FALSE,quote = FALSE)
  }
  #------------------------------------------------------------------------------------------------------------------
  # all types overdose
  all_types_overdose_matrix <<- generate_stochastic_all_types_overodse()
  
  if (save_input_files_as_csv == "yes")
  {
    factor_perm <- expand.grid(oud[1:(lmax/2)],sex,agegrp,block)
    colnames(factor_perm)<-c("oud","sex","agegrp","block")
    factor_perm <- factor_perm[,c("block","agegrp","sex","oud")]
    col_names <- c("oud","sex","agegrp","block")
    for (i in 1:length(time_varying_overdose_cycles))
    {
      col_names_tmp <- paste("all_types_overdose_c",time_varying_overdose_cycles[i],sep="")
      col_names <- c(col_names, col_names_tmp)
    }
    overdose_tbl <- data.frame(factor_perm,all_types_overdose_matrix)
    colnames(overdose_tbl) <- col_names
    write.table(overdose_tbl ,file=paste("inputs/all_types_overdose",run_id,".csv", sep=''),sep = ",", col.names = col_names, row.names = FALSE)
  }
  
  # fatal to all types overdose ratio  
  fatal_overdose_ratio <- generate_fatal_overdose()
  fatal_overdose_matrix <<- cbind(time_varying_overdose_cycles,fatal_overdose_ratio)
  
  if (save_input_files_as_csv == "yes")
  {
    col_names <- c("cycles_inclusive","fatal_to_all_types_overdose_ratio")
    write.table(fatal_overdose_matrix ,file=paste("inputs/fatal_overdose",run_id,".csv", sep=''),sep = ",", col.names = col_names, row.names = FALSE)
  }
  # -----------------------------------------------------------------------------------------------------------------
  # OUD transition
  oud_trans_matrix <<- generate_oud_trans_matrix()
  if (save_input_files_as_csv == "yes")
  {
    factor_perm <- expand.grid(oud,sex,agegrp,block, stringsAsFactors = FALSE) 
    colnames(factor_perm) <- c("initial_status","sex","agegrp","block")
    factor_perm <- factor_perm[,c("block","agegrp","sex","initial_status")]
    col_names <- c("initial_status","sex","agegrp","block")
    for (i in 1:lmax)
    {
      col_names_tmp <- c(paste("to_",oud[i],sep=""))
      col_names <- c(col_names, col_names_tmp)
    }
    oud_trans_tbl <- data.frame(factor_perm,oud_trans_matrix)
    colnames(oud_trans_tbl) <- col_names
    write.csv(oud_trans_tbl, file=paste("inputs/oud_trans", run_id, ".csv", sep=""),row.names = FALSE,quote = FALSE)
  }
  #------------------------------------------------------------------------------------------------------------------- 
  # Block transition
  block_trans_matrix <<- generate_block_trans_matrix()
  if (save_input_files_as_csv == "yes")
  {
    factor_perm<-expand.grid(block,oud,sex,agegrp) 
    colnames(factor_perm)<-c("initial_trt","oud","sex","agegrp")
    factor_perm<-factor_perm[,c("agegrp","sex","oud","initial_trt")]
    col_names <- c("initial_trt","sex","agegrp","block")
    
    for (i in 1: (ceiling(imax/2)))
    {
      col_names_tmp <- c(paste("to_",block[i],sep=""))
      col_names <- c(col_names, col_names_tmp)
    }
    col_names <- c(col_names, "to_corresponding_post_trt")
    block_trans_tbl <- data.frame(factor_perm,block_trans_matrix)
    colnames(block_trans_tbl) <- col_names
    write.csv(block_trans_tbl,file=paste("inputs/block_trans",run_id,".csv",sep=""),row.names = FALSE,quote = FALSE)
  }

  # Block initiation effect
  block_init_effect_matrix <<- generate_block_init_matrix()
  if (save_input_files_as_csv == "yes")
  {
    df_tmp <- data.frame(oud)
    col_names <- c("initial_oud_state")
    for (i in 1:imax)
    {
      col_names <- c(col_names,paste("to_", block[i],sep=""))
    }
    blk_init_eff_tbl <- data.frame(df_tmp,block_init_effect_matrix)
    colnames(blk_init_eff_tbl) <- col_names
    write.csv(blk_init_eff_tbl,file=paste("inputs/block_init_effect",run_id,".csv",sep=""),row.names = FALSE,quote = FALSE)
  }
  
}
  
