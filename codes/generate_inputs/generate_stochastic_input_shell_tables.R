# Generate the shell tables based on number of compartments
# These tables can be manually filled by user later
generate_stochastic_input_shell_tables <- function()
{
  # Entering cohort
  factor_perm<-expand.grid(sex,agegrp) 
  colnames(factor_perm)<-c("sex","agegrp")
  factor_perm<-factor_perm[,c("agegrp","sex")]
  dist <- rep("", jmax*kmax)
  param1 <- rep("", jmax*kmax)
  param2 <- rep("", jmax*kmax)
  
  for (i in 1:(length(time_varying_entering_cohort_cycles)))
  {
    df_tmp <- data.frame(dist,param1,param2)
    col_names_tmp <- c(paste("proportion_of_new_comers_dist#c",time_varying_entering_cohort_cycles[i],sep=""),paste("number_of_new_comers_param1_c",time_varying_entering_cohort_cycles[i],sep=""),paste("number_of_new_comers_param2_c",time_varying_entering_cohort_cycles[i],sep=""))
    colnames(df_tmp) <- col_names_tmp
    factor_perm <- cbind(factor_perm,df_tmp)
  }
  write.csv(factor_perm,file="inputs/entering_cohort.csv",row.names = FALSE,quote = FALSE)
  #------------------------------------------------------------------------------------------------
  # SMR 
  factor_perm<-expand.grid(oud,sex,agegrp,block) 
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  dist <- rep("", total_num_compartments)
  param1 <- rep("", total_num_compartments)
  param2 <- rep("", total_num_compartments)
  SMR_tbl <- data.frame(factor_perm,dist,param1,param2)
  colnames(SMR_tbl) <- c("block","agegrp","sex","oud","SMR_dist","SMR_param1","SMR_param2")
  write.csv(SMR_tbl,file="inputs/SMR.csv",row.names = FALSE,quote = FALSE)
  # -----------------------------------------------------------------------------------------------------------------
  # All types overdose
  factor_perm<-expand.grid(oud[1:(lmax/2)],sex,agegrp,block) 
  colnames(factor_perm)<-c("oud","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
  dist <- rep("", total_num_compartments/2)
  param1 <- rep("", total_num_compartments/2)
  param2 <- rep("", total_num_compartments/2)

  for (i in 1:(length(time_varying_overdose_cycles)))
  {
    df_tmp <- data.frame(dist,param1,param2)
    col_names_tmp <- c(paste("all_types_overdose_dist#c",time_varying_overdose_cycles[i],sep=""),paste("all_types_overdose_param1#c",time_varying_overdose_cycles[i],sep=""),paste("all_types_overdose_param2#c",time_varying_overdose_cycles[i],sep=""))
    colnames(df_tmp) <- col_names_tmp
    factor_perm <- cbind(factor_perm,df_tmp)
  }
  write.csv(factor_perm,file="inputs/all_types_overdose.csv",row.names = FALSE,quote = FALSE)
  
  # Fatal to all_types overdose ratio, time_varying but not stratified
  fatal_overdose_ratio_dist <- rep("", length(time_varying_overdose_cycles))
  fatal_overdose_ratio_param1<- rep("", length(time_varying_overdose_cycles))
  fatal_overdose_ratio_param2 <- rep("", length(time_varying_overdose_cycles))
  col_names_tmp <- matrix(rep("",length(time_varying_overdose_cycles)*3), nrow=1)
  it <- 0
  for (i in 1:(length(time_varying_overdose_cycles)))
  {
    it <- it+1
    col_names_tmp[1,it] <- paste("fatal_to_all_types_overdose_ratio_dist#c",time_varying_overdose_cycles[i],sep="")
    it <- it+1
    col_names_tmp[1,it] <- paste("fatal_to_all_types_overdose_ratio_param1#c",time_varying_overdose_cycles[i],sep="")
    it <- it+1
    col_names_tmp[1,it] <- paste("fatal_to_all_types_overdose_ratio_param2#c",time_varying_overdose_cycles[i],sep="")
  }
  write.table(col_names_tmp,file="inputs/fatal_overdose.csv",row.names = FALSE,quote = FALSE, col.names = FALSE, sep=",")
  #------------------------------------------------------------------------------------------------------------------
  # OUD transition
  factor_perm<-expand.grid(oud,sex,agegrp,block, stringsAsFactors = FALSE) 
  colnames(factor_perm)<-c("initial_status","sex","agegrp","block")
  factor_perm<-factor_perm[,c("block","agegrp","sex","initial_status")]
  dist <- rep("", total_num_compartments)
  param1 <- rep("", total_num_compartments)
  param2 <- rep("", total_num_compartments)
  
  for (i in 1:lmax)
  {
    df_tmp <- data.frame(dist,param1,param2)
    col_names <- c(paste("to_",oud[i],"_dist",sep=""),paste("to_",oud[i],"_param1",sep=""),paste("to_",oud[i],"_param2",sep=""))
    colnames(df_tmp) <- col_names
    factor_perm <- data.frame(factor_perm,df_tmp)
  }
  write.csv(factor_perm, file="inputs/oud_trans.csv",row.names = FALSE,quote = FALSE)
  # --------------------------------------------------------------------------------------------------------------------
  # Block transition
  factor_perm<-expand.grid(block,oud,sex,agegrp) 
  colnames(factor_perm)<-c("initial_block","oud","sex","agegrp")
  factor_perm<-factor_perm[,c("agegrp","sex","oud","initial_block")]
  
  for (i in 1: (ceiling(imax/2)))
  {
    df_tmp <- data.frame(dist,param1,param2)
    col_names<-c(paste("to_",block[i],"_dist",sep=""),paste("to_",block[i],"_param1",sep=""),paste("to_",block[i],"_param2",sep=""))
    colnames(df_tmp)<-col_names
    factor_perm<-data.frame(factor_perm,df_tmp)
  }
  df_tmp <- data.frame(dist,param1,param2)
  colnames(df_tmp) <- c("to_corresponding_post_trt_dist","to_corresponding_post_trt_param1","to_corresponding_post_trt_param2")
  factor_perm<-data.frame(factor_perm,df_tmp)
  write.csv(factor_perm,file="inputs/block_trans.csv",row.names = FALSE,quote = FALSE)
  
  # Block inititation effect table
  to_trt <- rep("",lmax)
  blk_init_eff_tbl <- data.frame(oud)
  colnames(blk_init_eff_tbl) <- c("initial_oud_state")
  
  for (i in 1:imax)
  {
    df_tmp <- data.frame(to_trt,to_trt,to_trt)
    col_names <- c(paste("to_", block[i],"_dist",sep=""),paste("to_", block[i],"_param1",sep=""),paste("to_", block[i],"_param2",sep=""))
    colnames(df_tmp) <- col_names
    blk_init_eff_tbl <- cbind(blk_init_eff_tbl, df_tmp)
  }
  write.csv(blk_init_eff_tbl,file="inputs/block_init_effect.csv",row.names = FALSE,quote = FALSE)
}
# ------------------------------------------------------------------------------------------------------------------
  