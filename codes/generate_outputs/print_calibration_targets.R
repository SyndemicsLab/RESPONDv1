# assuming weekly cycles and annual non-stratified targets for 3 years
print_calibration_targets <- function()
{
  num_cycles_per_year <- 52
  targets <- matrix(nrow = 3, ncol = 3)
  col_names <-  c("prevalence","fatal_overdose","admissions_to_detox")
  if (detox_block_id == -1)
  {
    targets <- matrix(nrow = 3, ncol = 2)
    col_names <-  c("prevalence","fatal_overdose")
  }

  if (detox_block_id == -1)
  {
    for (c in 1:3)
    {
      targets[c,1] <- sum(out$general_outputs[c*num_cycles_per_year+1,])
      targets[c,2] <- sum(out$overdose_outputs[((c-1)*num_cycles_per_year+2):(c*num_cycles_per_year+1),])*fatal_overdose_vec[c]
    } 
  } else {
    for (c in 1:3)
    {
      targets[c,1] <- sum(out$general_outputs[c*num_cycles_per_year+1,])
      targets[c,2] <- sum(out$overdose_outputs[((c-1)*num_cycles_per_year+2):(c*num_cycles_per_year+1),])*fatal_overdose_vec[c]
      targets[c,3] <- sum(out$admission_to_trts[((c-1)*num_cycles_per_year+2):(c*num_cycles_per_year+1),detox_block_id])
    }
  }
  targets_df <- data.frame(targets)
  colnames(targets_df) <- col_names
  write.csv(targets_df,file=paste("./output",strategy_id,"/calibration_targets",run_id,".csv",sep=""),row.names = FALSE, quote = FALSE)
}
