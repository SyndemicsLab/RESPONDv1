### General inputs ###
# These are global variables, so all of them should have <<- instead of <-

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
simulation_duration <<- 9

# Aging parameters
cycles_in_age_brackets <<- 5      # number of cycles in each age bracket. With weekly cycle and 5-year age bracket: 52*5

# entering cohort parameters
time_varying_entering_cohort_cycles <<- c(6,9,12)    # time intervals for entering cohort. Only the upper limit is inclusive.

# block transition parameters
time_varying_blk_trans_cycles <<- c(3,10) # each element should have its own matrix. Each of thses matrices represents a time interval. Only the upper limit is inclusive.

# overdose parameters
# all types overdose and fatal to all types overdose ratios are considered to have the same time_varying intervals
time_varying_overdose_cycles <<- c(6,9,12)    # each element should have its own column(for all types) or row (for fatal). Each of them represents a time interval, from last cycle to current cycle. Only the upper limit is inclusive

# Inidicate whether you want to inlcude cost analysis here
cost_analysis <<- "yes"   # enter "no" for calibration mode
cost_perspectives <<- c("healthcare_system","societal","policy_makers")  # cost perspectives to be included in cost analysis

discounting_rate <<- 0.0025
# OUTPUT OPTIONS
# ---------------------------------------------------------------------------------------------------------------------
# Number of cycles in desired intervals for combining outputs. E.g. annual interval with weekly cycles has period of 52.
# remainder of duration/periods should be 0.
# This parameter is also used for calculating accumulated cost/life
periods <<- 3

# For debugging purpose, if you need to reformat the output of the simulation to 1 file per block output, use the following flag
print_per_blk_output <<- "yes"      # "yes" or "no"

# Save general outputs (all compartments' sizes in each cycle), all types overdose, background mortality and admissions to treatment episodes as csv files
print_general_outputs <<- "yes"      # "yes" or "no"

# This function generates the general statistics which includes total size, number of males (sex_ID=1) and
# number of each OUD status of each block in chosen cycles.
# Input is a vector of cycles for general stats.
# If the input is an empty vector, e.g. c() with length of zero, this function will not be called.
general_stats_cycles <<- c(0,6,9)

# If "yes", all cost categories will be printed out and saved as .csv files.
# If cost_analysis is "yes" total cost per block per perspective per interval will be printed anyway.This option will provide additional outputs.
print_cost_categories <<- "yes"
