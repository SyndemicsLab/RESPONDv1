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

# load general user inputs
source("inputs/user_inputs.R")

# Set the seed if user has chosen an acceptable one.
if (seed != -1)
{
  set.seed(seed) 
}

if (run_id_type == "external")
{
  if ("getopt" %in% rownames(installed.packages()) == FALSE)
  {
    install.packages("getopt",repos = "http://cran.us.r-project.org")
  } else {
    library(getopt)
  }
  args <- commandArgs(trailingOnly=TRUE)
  run_id <- as.numeric(args[1])
}

# open a file to sink the errors
msgcon <- file(paste("errors",run_id,".txt",sep=""), open = "w")
sink(msgcon , append = FALSE, type = c("message"),
     split = FALSE)

# check the general user inputs
source("codes/generate_inputs/check_general_inputs.R")
check_general_inputs()

# source files
sourceCpp("codes/simulation.cpp")
source("codes/generate_outputs/generate_output_IDs.R")
generate_output_IDs()

# choose input type and then create them
if (input_type == "deterministic")
{
  source("codes/generate_inputs/load_inputs.R")
  load_inputs()
} else {
  source("codes/generate_inputs/generate_inputs.R")
  source("codes/generate_inputs/tmp/simulate_deterministic_user_inputs.R")  #temporary, to be removed in the final version
  simulate_deterministic_user_inputs()  #temporary, to be removed in the final version
  generate_inputs()
}

# check final inputs of the simulation
source("codes/generate_inputs/check_load_or_generated_inputs.R")
check_load_gen_inputs()

# If there is any warning in inputs, stop and print the warning messages.
if (length(warnings()) != 0)
{
  quit(save="no")
}

### calling simulation function
# "out" is the ouptput of the simulation. Each row is a cyle (starting from cycle 0 for initial stats) and each
# column is number of persons within each compartment. Compartments are in the increasing order based on their output IDs. 
# "out" can be used to analyze the outputs and plot graphs.
out<<- sim (
     init_demographics_vec,
     entering_cohort_matrix, time_varying_entering_cohort_cycles,
     oud_trans_matrix,
     block_trans_matrix, block_init_effect_matrix, detox_block_id,
     all_types_overdose_matrix, fatal_overdose_matrix,
     mort_vec,
     imax,jmax, kmax,lmax,
     simulation_duration,
     cycles_in_age_brackets)

if (save_general_outputs == "yes")
{
  write.table(out$`general outputs`, file = paste("outputs/general_outputs",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = general_IDs)
  write.table(out$`overdose outputs`, file = paste("outputs/all_types_overdose",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = active_oud_IDs)
  write.table(out$`mortality outputs`, file = paste("outputs/background_mortality",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = general_IDs)
  if (imax > 1)
  {
    write.table(out$`admission to trts`, file = paste("outputs/admission_to_trts",run_id,".csv",sep = ""),sep=",",row.names = FALSE,quote = FALSE, col.names = block_idx[2:ceiling(imax/2)])
  }
}

if (write_per_trt_output == "yes")
{
  source("codes/generate_outputs/write_outputs_per_block.R")
  write_outputs_per_block()
}

if (length(general_stats_cycles) != 0)
{
  source("codes/generate_outputs/get_general_stats.R")
  get_general_stats_in_cycle(general_stats_cycles)
}





