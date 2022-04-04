### General inputs ###
# These are global variables, so all of them should have <<- instead of <-

# block names and order:
# no_treatment always is considered as trt0.
# treatment episodes would follow no_treatment. current order of trt: inpatient, outpatient
# then post_treatment episodes are added in the same order as treatments.
## For the three treatments: "Buprenorphine","Naltrexone", and "Methadone"
## the other acceptable naming formats are "Bup", "Ntx"/"Nal" and "Mmt"/"Meth" respectively
## Above naming conventions are not case sensitive and the user is required to follow these
## to do stochastic runs for OUD transitions with Multi state models (MSM)
## Also, note that for any runs post treatment episode names should be separated by a dash
block <<- c("No_Treatment","Buprenorphine","Naltrexone","Methadone","Detox",
            "Post-Buprenorphine","Post-Naltrexone","Post-Methadone","Post-Detox")

# age brackets, always from youngest to oldest
## For stochastic runs, the age groups should only be separate by
## an underscore i.e., "_'. The user cannot use any other symbols in between the age groups
agegrp <<- c("10_14","15_19","20_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59","60_64","65_69",
             "70_74","75_79","80_84","85_89","90_94","95_99")

#gender groups, "m" or "male" is the reference group, so it should always come first
sex <<- c("Male","Female")

# OUD states: active states always come first. Then add non_active states in the same order as active ones.
## For stochastic runs oud states should start with either active or nonactive then followed by the route info: inj vs noninj
## In other words, first part of the name should be either active or nonacitive
## you can choose either to separate the two parts with a symbol like "-" or not. That doesn't affect the stochastic run
## implementation
## For LHS runs OUD state has to be specified as the four states
## "c("Active_Noninjection","Active_Injection","Nonactive_Noninjection","Nonactive_Injection")". However, it is not case sensitive.

oud <<- c("Active_Noninjection","Active_Injection","Nonactive_Noninjection","Nonactive_Injection")

# Duration of simulation in cycles. remainder of duration/periods should be 0.
simulation_duration <<- 156

# Aging parameters
cycles_in_age_brackets <<- 260      # number of cycles in each age bracket. With weekly cycle and 5-year age bracket: 52*5=260

# entering cohort parameters
time_varying_entering_cohort_cycles <<- c(52,104,156)   # time intervals for entering cohort. Only the upper limit is inclusive.

# block transition parameters
## Currently, for stochastic runs this should always be c(156) where 156 is the simulation duration
## as block transitions are not time varying for stochastic runs
##  Block transitions are time varying only for strategy vise runs
time_varying_blk_trans_cycles <<- c(52,104,156) # each element should have its own matrix. Each of these matrices represents a time interval. Only the upper limit is inclusive.

# overdose parameters
# all types overdose and fatal to all types overdose ratios are considered to have the same time_varying intervals
time_varying_overdose_cycles <<- c(52,104,156)   # each element should have its own column(for all types) or row (for fatal). Each of them represents a time interval, from last cycle to current cycle. Only the upper limit is inclusive

# Indicate whether you want to include cost analysis here
cost_analysis <<- "no"   # enter "no" for calibration mode
cost_perspectives <<- c("healthcare_system","societal","policy_makers")  # cost perspectives to be included in cost analysis

discounting_rate <<- 0.0025
# OUTPUT OPTIONS
# ---------------------------------------------------------------------------------------------------------------------
# Number of cycles in desired intervals for combining outputs. E.g. annual interval with weekly cycles has period of 52.
# remainder of duration/periods should be 0.
# This parameter is also used for calculating accumulated cost/life
periods <<- 52

# For debugging purpose, if you need to reformat the output of the simulation to 1 file per block output, use the following flag
print_per_blk_output <<- "no"      # "yes" or "no"

# Save general outputs (all compartments' sizes in each cycle), all types overdose, background mortality and admissions to treatment episodes as csv files
print_general_outputs <<- "yes"      # "yes" or "no"

# This function generates the general statistics which includes total size, number of males (sex_ID=1) and
# number of each OUD status of each block in chosen cycles.
# Input is a vector of cycles for general stats.
# If the input is an empty vector, e.g. c() with length of zero, this function will not be called.
general_stats_cycles <<- c(52,104,156)

# If "yes", all cost categories will be printed out and saved as .csv files.
# If cost_analysis is "yes" total cost per block per perspective per interval will be printed anyway.This option will provide additional outputs.
print_cost_categories <<- "no"
