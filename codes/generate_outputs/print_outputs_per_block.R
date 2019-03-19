### separating outputs for each block and writing to .csv files inside "outputs" folder

print_outputs_per_block <- function() {
  num_comp_per_block <- jmax*kmax*lmax
  n.rows <- simulation_duration+1;  # each row represents one simulation cycle. There is an additional row for cycle 0 (initial status)
  for (i in 1:imax)
  {
    general_output_name <- paste("outputs/counts_block",i-1,".csv", sep='')
    mortality_output_name <- paste("outputs/bg_mortality_block",i-1,".csv", sep='')

    first_column <- num_comp_per_block*(i-1)+1
    last_column <- first_column + num_comp_per_block -1
    # printing general counts per block
    write.csv(out$general_outputs[1:n.rows,first_column:last_column],file=general_output_name, row.names = c(0:(n.rows-1)), quote = FALSE)
    # printing mortality outputs per block
    write.csv(out$mortality_outputs[1:n.rows,first_column:last_column],file=mortality_output_name, row.names = c(0:(n.rows-1)), quote = FALSE)
    }
  
  num_comp_per_block <- jmax*kmax*lmax/2
  for (i in 1:imax)
  {
    overdose_output_name <- paste("outputs/all_overdose_block",i-1,".csv", sep='')
    first_column <- num_comp_per_block*(i-1)+1
    last_column <- first_column + num_comp_per_block -1
    # printing overdose outputs per block
    write.csv(out$overdose_outputs[1:n.rows,first_column:last_column],file=overdose_output_name, row.names = c(0:(n.rows-1)), quote = FALSE)
  }
}
