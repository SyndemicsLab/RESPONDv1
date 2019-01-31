# Generate the shell tables based on number of compartments
# These tables can be manually filled by user later
generate_deterministic_input_shell_tables <- function()
{
  # initial cohort
  factor_perm<-expand.grid(oud,sex,agegrp,block[1:ceiling((imax/2))])
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  num_compartments <- ceiling(imax/2)*jmax*kmax*lmax
  counts <- rep("", num_compartments)
  init_cohort <- data.frame(factor_perm,counts)
  write.csv(init_cohort,file="inputs/init_cohort.csv",row.names = FALSE,quote = FALSE)
  # -----------------------------------------------------------------------------------------------------------------
  # entering cohort
  factor_perm <- expand.grid(sex,agegrp)
  colnames(factor_perm)<-c("sex","agegrp")
  factor_perm<-factor_perm[,c("agegrp","sex")]
  counts <- rep("", jmax*kmax)
  
  for (i in 1:(length(time_varying_entering_cohort_cycles)))
  {
    df_tmp <- data.frame(counts)
    col_names_tmp <- c(paste("number_of_new_comers_c",time_varying_entering_cohort_cycles[i],sep=""))
    colnames(df_tmp) <- col_names_tmp
    factor_perm <- cbind(factor_perm,df_tmp)
  }
  write.csv(factor_perm,file="inputs/entering_cohort.csv",row.names = FALSE,quote = FALSE)
  # -----------------------------------------------------------------------------------------------------------------
  # background mortality
  factor_perm<-expand.grid(sex,agegrp)
  colnames(factor_perm)<-c("sex","agegrp")
  factor_perm<-factor_perm[,c("agegrp","sex")]
  num_compartments <- jmax*kmax
  death_prob <- rep("", num_compartments)
  bg_mortality <- data.frame(factor_perm,death_prob)
  write.csv(bg_mortality,file="inputs/background_mortality.csv",row.names = FALSE,quote = FALSE)
  # -----------------------------------------------------------------------------------------------------------------
  # SMR 
  factor_perm<-expand.grid(oud,sex,agegrp,block) 
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  SMR <- rep("", total_num_compartments)
  SMR_tbl <- data.frame(factor_perm,SMR)
  colnames(SMR_tbl) <- c("block","agegrp","sex","oud","SMR")
  write.csv(SMR_tbl,file="inputs/SMR.csv",row.names = FALSE,quote = FALSE)
  # -----------------------------------------------------------------------------------------------------------------
  # All types overdose
  factor_perm<-expand.grid(oud[1:(lmax/2)],sex,agegrp,block) 
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  all_types_overdose <- rep("", total_num_compartments/2)
  
  for (i in 1:(length(time_varying_overdose_cycles)))
  {
    df_tmp <- data.frame(all_types_overdose)
    col_names_tmp <- c(paste("all_types_overdose_c",time_varying_overdose_cycles[i],sep=""))
    colnames(df_tmp) <- col_names_tmp
    factor_perm <- cbind(factor_perm,df_tmp)
  }
  write.csv(factor_perm,file="inputs/all_types_overdose.csv",row.names = FALSE,quote = FALSE)
  
  # Fatal to all_types overdose ratio, time_varying but not stratified
  # fatal to all types overdose ratio  
  # Fatal to all_types overdose ratio, time_varying but not stratified
  fatal_overdose_ratio <- rep("", length(time_varying_overdose_cycles))
  overdose_tbl <- data.frame(time_varying_overdose_cycles, fatal_overdose_ratio)
  colnames(overdose_tbl) <- c("cycles_inclusive","fatal_to_all_types_overdose_ratio")
  write.csv(overdose_tbl,file="inputs/fatal_overdose.csv",row.names = FALSE,quote = FALSE)
  #------------------------------------------------------------------------------------------------------------------
  # OUD transition
  factor_perm<-expand.grid(oud,sex,agegrp,block, stringsAsFactors = FALSE) 
  colnames(factor_perm)<-c("initial_status","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","initial_status")]
  
  for (i in 1:lmax)
  {
    values <- rep("", total_num_compartments)
    df_tmp <- data.frame(values)
    col_names <- c(paste("to_",oud[i],sep=""))
    colnames(df_tmp) <- col_names
    factor_perm <- data.frame(factor_perm,df_tmp)
  }
  write.csv(factor_perm, file="inputs/oud_trans.csv",row.names = FALSE,quote = FALSE)

  # --------------------------------------------------------------------------------------------------------------------
  # Block transition
  factor_perm<-expand.grid(block,oud,sex,agegrp) 
  colnames(factor_perm)<-c("initial_trt","oud","sex","agegrp")
  factor_perm<-factor_perm[,c("agegrp","sex","oud","initial_trt")]
  
  for (i in 1: (ceiling(imax/2)))
  {
    df_tmp <- data.frame(values)
    col_names<-c(paste("to_",block[i],sep=""))
    colnames(df_tmp)<-col_names
    factor_perm<-data.frame(factor_perm,df_tmp)
  }
  df_tmp <- data.frame(values)
  colnames(df_tmp) <- c("to_corresponding_post_trt")
  factor_perm<-data.frame(factor_perm,df_tmp)
  write.csv(factor_perm,file="inputs/block_trans.csv",row.names = FALSE,quote = FALSE)
  
  # Block inititation effect table
  to_trt <- rep("",lmax)
  blk_init_eff_tbl <- data.frame(oud)
  colnames(blk_init_eff_tbl) <- c("initial_oud_state")
  
  for (i in 1:imax)
  {
    df_tmp <- data.frame(to_trt)
    col_names <- paste("to_", block[i],sep="")
    colnames(df_tmp) <- col_names
    blk_init_eff_tbl <- cbind(blk_init_eff_tbl, df_tmp)
  }
  write.csv(blk_init_eff_tbl,file="inputs/block_init_effect.csv",row.names = FALSE,quote = FALSE)


}
