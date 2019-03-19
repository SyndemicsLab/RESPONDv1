### General inputs ###
# These are global variables, so all of them should have <<- instead of <-

# Assigning run type
# calibration:  Model is called from an external source (terminal), so run_id will be set by the external call to the main file
#               input_type should be "stochastic"
#               seed_type = "random" uses R to generate random seed. In this case, the results won't be reproducible
#               seed_type = "fixed" assigns run_id to the seed value to get reproducible runs
# analysis:     Model is called from an external source (terminal), so run_id will be set by the external call to the main file
#               Use for final analysis that usually require large number of runs
#               input_type should be "stochastic"
#               seed_type = "random" use R to generate random seed. In this case, the results won't be reproducible
#               seed_type = "fixed" assigns run_id to the seed value to get reproducible runs
# manual:       Model is called from Rstudio, so run_id should be set manually 
#               Use for small number of runs with the purpose of debugging or understanding the model
#               input_type can be either "deterministic" (no seed required) or "stochastic" (set the seed to a positive integer value if you choose fixed seed)
run_type <<-  "manual"       # "manual" or "calibration" or "analysis"

# Input type
# deterministic:      Inputs will be read as deterministic values from saved .csv files, so there is no need for a seed
#                     All input files should have the 'original name', meaning no run number included in their names
# stochastic:         Inputs are generated stochastically by code, so you need to set the seed. Refer to seed types above
input_type <<- "stochastic"     # Only for "manual" mode
run_id <<- 1                    # Only for "manual" mode

seed_type <<- "fixed"             # "fixed" or "random"
seed <<- 1                        # Only used in "manual stochastic" mode with "fixed" seed type

# For stochastic runs, the drawn inputs are directly passed to the simulation. Set this parameter to "yes" if you want to save these inputs as .csv on hard drive for further reference.
save_input_files_as_csv <<- "yes"

# number of digits used for rounding inputs
precision <<- 6

# block names and order:
# no_treatment always is considered as trt0.
# treatment episodes would follow no_treatment. current order of trt: inpatient, outpatient
# then post_treatment episodes are added in the same order as treatments.
block <<- c("no_trt","detox","res","mmt","bup","ntx","cor",
            "post_detox","post_res","post_mmt","post_bup","post_ntx","post_cor")

# age brackets, always from youngest to oldest
agegrp <<- c("10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59","60_64","65_69",
            "70_74","75_79","80_84","85_89","90_94","95_99")

#gender groups, "m" or "male" is the reference group, so it should always come first
sex <<- c("m","f")

# OUD states: active states always come first. Then add non_active states in the same order as active ones.
oud <<- c("active_noninj","active_inj","nonactive_noninj","nonactive_inj")

# Duration of simulation in cycles. remainder of duration/periods should be 0.
simulation_duration <<- 156

# Aging parameters
cycles_in_age_brackets <<- 260       # number of cycles in each age bracket. With weekly cycle and 5-year age bracket: 52*5

# entering cohort parameters
time_varying_entering_cohort_cycles <<- c(52,104,156)    # time intervals for entering cohort. Only the upper limit is inclusive.
entering_cohort_total_size <<- c(1000,2000,3000)      # total size of entering cohort in each cycle in each specific time interval. The length of this vector should be the same as length of time varying cycles

# overdose parameters
# all types overdose and fatal to all types overdose ratios are considered to have the same time_varying intervals
time_varying_overdose_cycles <<- c(52,104,156)    # each element should have its own column(for all types) or row (for fatal). Each of them represents a time interval, from last cycle to current cycle. Only the upper limit is inclusive

# Inidicate whether you want to inlcude cost analysis here
cost_analysis <<- "no"   # enter "no" for calibration mode
cost_perspectives <<- c("healthcare_system","societal","policy_makers")  # cost perspectives to be included in cost analysis

#overdose_utility <<- 0.8    # the same for both fatal and non-fatal overdose
discounting_rate <<- 0.0025
# OUTPUT OPTIONS
# ---------------------------------------------------------------------------------------------------------------------
# Number of cycles in desired intervals for combining outputs. E.g. annual interval with weekly cycles has period of 52.
# remainder of duration/periods should be 0.
# This parameter is also used for calculating accumulated cost/life
periods <<- 52

# For debugging purpose, if you need to reformat the output of the simulation to 1 file per block output, use the following flag
print_per_trt_output <<- "no"      # "yes" or "no"

# Save general outputs (all compartments' sizes in each cycle), all types overdose, background mortality and admissions to treatment episodes as csv files
print_general_outputs <<- "yes"      # "yes" or "no"

# This function generates the general statistics which includes total size, number of males (sex_ID=1) and
# number of each OUD status of each block in chosen cycles.
# Input is a vector of cycles for general stats.
# If the input is an empty vector, e.g. c() with length of zero, this function will not be called.
general_stats_cycles <<- c()

# If "yes", all cost categories will be printed out and saved as .csv files.
# If cost_analysis is "yes" total cost per block per perspective per interval will be printed anyway.This option will provide additional outputs.
print_cost_categories <<- "yes"
