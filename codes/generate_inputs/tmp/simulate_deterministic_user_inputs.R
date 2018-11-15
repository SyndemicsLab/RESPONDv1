simulate_deterministic_user_inputs <- function(){
  # initial cohort
  cohort_enlargement_factor <- 2
  init_demographics_vec <<-round(runif(ceiling(imax/2)*jmax*kmax*lmax,0,5000),0) 
  if (save_input_files_as_csv == "yes")
  {
    factor_perm<-expand.grid(oud,sex,agegrp,block[1:ceiling((imax/2))])
    colnames(factor_perm)<-c("oud","sex","agegrp","block")
    factor_perm<-factor_perm[,c("block","agegrp","sex","oud")]
    init_cohort <- data.frame(factor_perm,init_demographics_vec)
    colnames(init_cohort) <- c("block","agegrp","sex","oud","counts")
    write.csv(init_cohort,file="inputs/init_cohort.csv",row.names = FALSE,quote = FALSE)
  }
  # ------------------------------------------------------------------------------------------------------------------
  # mortality
  bg_mort <- round(runif(jmax*kmax,0.01,0.2),digits = 4)
  
  if (save_input_files_as_csv == "yes")
  {
    factor_perm<-expand.grid(sex,agegrp)
    colnames(factor_perm)<-c("sex","agegrp")
    factor_perm<-factor_perm[,c("agegrp","sex")]
    mortality_prob <- data.frame(factor_perm,bg_mort)
    colnames(mortality_prob) <- c("agegrp","sex","death_prob")
    write.csv(mortality_prob,file="inputs/background_mortality.csv",row.names = FALSE,quote = FALSE)
  }
  #-------------------------------------------------------------------------------------------------------------------
  
}