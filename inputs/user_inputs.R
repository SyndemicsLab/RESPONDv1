### General inputs ###
# These are global variables, so all of them should have <<- instead of <-

# Set the seed if you want to have reproducible code.
# set to -1 if you want R to generate random seed
seed <<- 1

# block names and order:
  # no_treatment always is considered as trt0.
  # treatment episodes would follow no_treatment. current order of trt: inpatient, outpatient
  # then post_treatment episodes are added in the same order as treatments.
block <<- c("no_trt","detox","res","mmt","bup","ntx","cor",
         "post_detox","post_res","post_mmt","post_bup","post_ntx","post_cor")

# age brackets, always from youngest to oldest
agegrp <<- c("10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59","60_64","65_69",
         "70_74","75_79","80_84","85_89","90_94","95_99")

#gender groups
sex <<- c("m","f")

# OUD states: active states always come first. Then add non_active states in the same order as active ones. 
oud <<- c("active_noninj","active_inj","nonactive_noninj","nonactive_inj")

# the inputs can be directly passed to simulation (useful for calibration or large runs) or be saved as .csv on hard drive for further reference.
save_input_files_as_csv <<- "yes"

# Inputs can be read as deterministic values from saved .csv files or be generated stochastically by code
# If "deterministic", then all the input file should have the 'original name', meaning no run number included in their names
input_type <<- "deterministic"    # "deterministic" or "stochastic"

# assigning the run_id. It could be assigned manually, by calibration process or by call from outside(external)
run_id_type <<-  "manual"                 # "manual" or "calibration" or "external"
run_id <<- 1                       # insert the run ID if manual type is chosen. It will be ignored if any other type is chosen

# For debugging purpose, if you need to reformat the output of the simulation to 1 file per block output, use the following flag
write_per_trt_output <<- "yes"      # "yes" or "no"

# Save the genral output (all compartments' sizes in each cycle) as a csv file
save_general_outputs <<- "yes"      # "yes" or "no"

# This function generates the general statistics which includes total size, number of males (sex_ID=1) and
# number of each OUD status of each block in chosen cycles.
# Input is a vector of cycles for general stats.
# If the input is an empty vector, e.g. c() with length of zero, this function will not be called.
general_stats_cycles <<- c(0)

# Duration of simulation in cycles
simulation_duration <<- 5

# Aging parameters
aging_prob <<- 1.0/(52*5);        # aging prob = 1/number of cycles in each age bracket. With weekly cycle and 5-year age bracket: 52*5
death_at_100yo <<- c(0.007,0.001)    # proportion of last age bracket who reach to 100 yo and automatically die. 1st element represents gender0 (male), 2nd element represents gender1 (female)

# entering cohort parameters
time_varying_entering_cohort_cycles <<- c(5,4,12)    # each element should have its own row for entering cohort values. Each row represents a time interval, from last row's cycle to current row's cycle. Only the upper limit is inclusive
# overdose parameters
# all types overdose and fatal to all types overdose ratios are considered to have the same time_varying intervals
time_varying_overdose_cycles <<- c(5,6,12)    # each element should have its own column(for all types) or row (for fatal). Each of them represents a time interval, from last cycle to current cycle. Only the upper limit is inclusive
