### The main code for RESPOND project that calls all the underlying functions to create the compartmental model,
### update and calibrate it.

# Set your current working directory here, if required.
# No need to set working directory if used as a project.
# all codes and inputs should be inside this folder in their specific format, name and path.
#setwd("~/workspace/RESPOND")

# Install required packages or load the libraries.
if("Rcpp" %in% rownames(installed.packages()) == FALSE) 
{
  install.packages("Rcpp",repos = "http://cran.us.r-project.org")
} else {
  library(Rcpp)
}

if ("getopt" %in% rownames(installed.packages()) == FALSE)
{
  install.packages("getopt",repos = "http://cran.us.r-project.org")
} else {
  library(getopt)
}

args <- commandArgs(trailingOnly=TRUE)
strategy_id <- as.numeric(args[1])
run_id <- as.numeric(args[2])

# load general user inputs
source(paste("input",strategy_id,"/input_file_paths.R", sep=""))
source(paste("input",strategy_id,"/user_inputs.R", sep=""))

# open a file to sink the errors
msgcon <- file(paste("errors_",strategy_id,"_",run_id,".txt",sep=""), open = "w")
sink(msgcon , append = FALSE, type = c("message"), split = FALSE)

# check the general user inputs
source("codes/generate_inputs/check_general_inputs.R")
check_general_inputs()

# load deterministic inputs
source("codes/generate_inputs/load_inputs.R")
load_inputs()

# check final inputs of the simulation
source("codes/generate_inputs/check_load_or_generated_inputs.R")
check_load_gen_inputs()

# source files
sourceCpp("codes/simulation.cpp")
source("codes/generate_outputs/generate_output_IDs.R")
generate_output_IDs()

### calling simulation function
# "out" is the ouptput of the simulation. Each row is a cycle (starting from cycle 0 for initial stats) and each
# column is number of persons within each compartment. Compartments are in the increasing order based on their output IDs. 
# "out" can be used to analyze the outputs and plot graphs.
out<<- sim (
     init_demographics_vec,
     entering_cohort_matrix, time_varying_entering_cohort_cycles,
     oud_trans_matrix,
     time_varying_blk_trans_cycles, block_trans_matrix, block_init_effect_matrix,
     time_varying_overdose_cycles, all_types_overdose_matrix, fatal_overdose_vec,
     mort_vec,
     imax,jmax, kmax,lmax,
     simulation_duration,
     cycles_in_age_brackets, periods,
     healthcare_utilization_cost, treatment_utilization_cost, pharmaceutical_cost,overdose_cost,discounting_rate)

# -------------------------------------------------------------------------------------------------------------------
# Print desired outputs based on user flags
if (print_general_outputs == "yes")
{
  write.table(out$general_outputs, file = paste("./output",strategy_id,"/general_outputs",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = general_IDs)
  write.table(out$overdose_outputs, file = paste("./output",strategy_id,"/all_types_overdose",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = active_oud_IDs)
  write.table(out$mortality_outputs, file = paste("./output",strategy_id,"/background_mortality",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = general_IDs)
  if (imax > 1)
  {
    write.table(out$admission_to_trts, file = paste("./output",strategy_id,"/admission_to_trts",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = block[2:ceiling(imax/2)])
  }
}

if (cost_analysis == "yes")
{
  source("codes/generate_outputs/print_costs.R")
  print_costs()
}

if (print_per_blk_output == "yes")
{
  source("codes/generate_outputs/print_outputs_per_block.R")
  print_outputs_per_block()
}

if (length(general_stats_cycles) != 0)
{
  source("codes/generate_outputs/get_general_stats.R")
  get_general_stats_in_cycle(general_stats_cycles)
}
