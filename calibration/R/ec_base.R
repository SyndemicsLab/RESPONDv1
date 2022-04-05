### make_input.R
### ============
### provided the data for the calibrated base case, create the input tables necessary
### for the RESPOND model to be run, choosing a random set of calibrated data from the
### pool of valid run options, recording which was chosen, in case it must be referenced
### later on

packages <- c("dplyr")
for (package in packages) {
  if (!(package %in% rownames(installed.packages()))) {
    install.packages(package, repos = "http://cran.us.r-project.org")
  }
  library(package, character.only = TRUE, warn.conflicts = FALSE)
}

## source utility functions
source("./calibration/R/utility.R")

## capture command line arguments
arguments             <- commandArgs(trailingOnly = TRUE)
## the resultant RESPOND input folder name
in_name               <- paste0("input", arguments[1])

### input files
## read in the calibration parameter dataset
calibrated_parameters <- read_params("./calibration/data/ec_parameters.csv")

## read in the fixed parameter datasets for tables that require scaling or formatting changes
raw_entering_cohort   <- read_params("./calibration/data/fixed/entering_cohort_priors.csv")
raw_init_effect       <- read_params("./calibration/data/fixed/init_effect_priors.csv")
raw_state_transition  <- read_params("./calibration/data/fixed/oud_trans_priors.csv")
raw_block_transition  <- read_params("./calibration/data/fixed/block_trans_priors.csv")
raw_overdose          <- read_params("./calibration/data/fixed/all_types_overdose_priors.csv")

## define the size of each age bin
age_bin_width         <- 5

### choosing a set of calibrated parameters
## each row of the calibration parameters is one valid run dataset, number of rows is
## choose a set (row) of parameters from the calibration parameter sets
## storing this so users know what data they used if they need to redo a run
chosen_set_num        <- sample(1:length(calibrated_parameters[,1]), size = 1)
chosen_set            <- calibrated_parameters[chosen_set_num,]

### constructing input tables
## formatting and structuring data based on calibration parameters

## ENTERING COHORT
## the entering cohort data has a yearly multiplier from the raw, fixed values to usable values
## calibrated entering cohort multiplier columns
cal_ec                <- chosen_set[ grepl("ent_cohort_counts", names(chosen_set)) ]
tmp_entering_cohort   <- process_entering(raw_entering_cohort, cal_ec, colnames(cal_ec))
entering_cohort       <- data.frame(matrix(nrow = 0, ncol = length(tmp_entering_cohort)))
colnames(entering_cohort) <- colnames(tmp_entering_cohort)

## break entering cohort into the same age strata as specified in user_inputs
for (i in 1:length(tmp_entering_cohort[,1])) {
  curr <- tmp_entering_cohort[i,]
  ages <- unlist(strsplit(curr[,"agegrp"], split = '_'))
  group_bounds <- as.numeric(ages)
  new_ages <- seq(group_bounds[1], group_bounds[2], by = age_bin_width)
  for (lower in new_ages) {
    row <- length(entering_cohort[,1]) + 1
    entering_cohort[row,] <- character(length(entering_cohort))
    entering_cohort[row, "agegrp"] <- paste0(lower,'_',lower+age_bin_width-1)
    entering_cohort[row, "sex"] <- curr[,"sex"]
    for (val in colnames(curr)[ grepl("number_of_new_comers_cycle", colnames(entering_cohort)) ]) {
      entering_cohort[row, val] <- curr[,val]/length(new_ages)
    }
  }
}

entering_cohort       <- entering_cohort %>% arrange(agegrp, desc(sex))

## INITIATION EFFECT
## initiation effects are broken into fixed and calibrated values -- this unifies the two.
## creating the table for the unified initiation effects
init_effect           <- data.frame(initial_oud_state = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"), to_No_Treatment = c(1,1,1,1))
## isolating the calibrated parameters relevant to initiation effect
cal_init_effect       <- chosen_set[grepl("block_init.", names(chosen_set))]
## adding the treatments to the table in the order they appear in the fixed parameters set
for (i in 1:length(raw_init_effect[,1])) {
  ## for post-treatment, replaces '-' with '.' as is done in the simulation
  treat_name <- paste("to_", gsub('-', '.', raw_init_effect[i,1]), sep = '')
  if (startsWith(treat_name, "to_Post")) {
    ## stratified by injection and noninjection, two values in trt
    ## it is assumed that in post-treatment active use is constant
    trt <- cal_init_effect[grepl(treat_name, names(cal_init_effect))]
    init_effect[treat_name] <- as.numeric(c(1, 1, trt))
  }
  else {
    ## single-value, fixed parameter in trt -- must be repeated
    ## it is assumed that in treatment, non-active states are constant
    trt <- raw_init_effect[i,"param1"]
    init_effect[treat_name] <- c(trt, trt, 1, 1)
  }
}

## OUD TRANS
## oud use state transitions - no treatment and post-treatments are calibrated
cal_state_transition  <- chosen_set[ grepl("oud_trans", names(chosen_set)) ]

## initialize the output oud_trans table as an empty table and name its columns
tmp_state_transition  <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(tmp_state_transition) <- c("block", "agegrp", "sex", "initial_status", "to_Active_Noninjection", "to_Active_Injection", "to_Nonactive_Noninjection", "to_Nonactive_Injection")

## add in the base, calibrated, no-treatment block
for (i in seq(1, length(cal_state_transition)/2, 2)) {
  row <- length(tmp_state_transition[,1])
  states <- c(cal_state_transition[i], cal_state_transition[i+1])
  to_states <- character()
  for (state in names(states)) {
    curr <- unlist(strsplit(state, '\\.'))
    to_states <- c(to_states, curr[5])
  }
  initial_state <- gsub("from_", "", curr[4])
  age <- curr[3]
  block <- curr[2]

  ## determine the third nonzero column - one column is always zero due
  ## to the design of transitions in the model
  states <- c(states, 1 - sum(as.numeric(states)))
  to_states <- c(to_states, oud_state_col(initial_state, to_states))

  for (j in 1:2) {
    ## allocate an empty row to store the current values
    tmp_state_transition[row + j,]     <- character(length(tmp_state_transition))
    ## assign corresponding values to the new row
    tmp_state_transition[row + j, 1]   <- block
    tmp_state_transition[row + j, 2]   <- age
    tmp_state_transition[row + j, 3]   <- ifelse(j %% 2 == 0, "Female", "Male")
    tmp_state_transition[row + j, 4]   <- initial_state
    tmp_state_transition[row + j, 5:8] <- "0"
    for (col in 1:length(to_states)) {
      tmp_state_transition[row + j, to_states[col]] <- as.character(states[col])
    }
  }
}

## add in treatment block oud state transitions from the fixed table provided
## it appears there's an assumption you cannot switch between active use types
for (i in 1:length(raw_state_transition[,1])) {
  row <- length(tmp_state_transition[,1])
  dat <- raw_state_transition[i,]
  for (j in 1:2) {
    ## injection or noninjection
    inject <- ifelse(j %% 2 == 0, "_Injection", "_Noninjection")

    ## allocate an empty row to store the current values
    tmp_state_transition[row + j,]     <- character(length(tmp_state_transition))
    tmp_state_transition[row + j,1]    <- dat[1]
    tmp_state_transition[row + j,2]    <- dat[2]
    tmp_state_transition[row + j,3]    <- dat[3]
    tmp_state_transition[row + j,4]    <- paste0(dat[4], inject)
    tmp_state_transition[row + j, 5:8] <- "0"
    ## the calibration value for the given state - Active or Nonactive
    probability <- cal_to_dist(as.character(dat["distribution"]),
                               as.numeric(dat["param1"]),
                               as.numeric(dat["param2"]))
    ## active states flip the indices
    if (dat[4] == "Active") {
      m <- 7
      n <- 5
    }
    else {
      m <- 5
      n <- 7
    }
    if (inject == "_Injection") {
      tmp_state_transition[row + j, m + 1] <- probability
      tmp_state_transition[row + j, n + 1] <- 1 - probability
    }
    else {
      tmp_state_transition[row + j, m] <- probability
      tmp_state_transition[row + j, n] <- 1 - probability
    }
  }
}

## in detox, initiation effects force all states to non-active, so
## it is acceptable to simply keep all states static (1s down the diagonal)
states                <- c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")
ages                  <- c("10_19", "20_24", "25_39", "40_54", "55_99")
for (grp in ages) {
  for (sex in c("Male", "Female")) {
    row <- length(tmp_state_transition[,1])
    for (j in 1:4) {
      tmp_state_transition[row + j,]     <- character(length(tmp_state_transition))
      tmp_state_transition[row + j,1]    <- "Detox"
      tmp_state_transition[row + j,2]    <- grp
      tmp_state_transition[row + j,3]    <- sex
      tmp_state_transition[row + j,4]    <- states[j]
      tmp_state_transition[row + j, 5:8] <- "0"
      tmp_state_transition[row + j, paste0("to_", states[j])] <- "1"
    }
  }
}

## the same process as for no-treatment, but with post-treatment states
## store the names of treatments, as these are not cached as of yet
## and the post-treatment values are identical regardless of treatment
treatments <- c(levels(factor(raw_state_transition$block)), "Detox")
for (treatment in treatments) {
  for (i in seq(length(cal_state_transition)/2+1, length(cal_state_transition), 2)) {
    row <- length(tmp_state_transition[,1])
    states <- c(cal_state_transition[i], cal_state_transition[i+1])
    to_states <- character()
    for (state in names(states)) {
      curr <- unlist(strsplit(state, '\\.'))
      to_states <- c(to_states, curr[5])
    }
    initial_state <- gsub("from_", "", curr[4])
    age <- curr[3]
    block <- paste0("Post-", treatment)

    ## determine the third nonzero column
    states <- c(states, 1 - sum(as.numeric(states)))
    to_states <- c(to_states, oud_state_col(initial_state, to_states))

    for (j in 1:2) {
      ## allocate an empty row to store the current values
      tmp_state_transition[row + j,]     <- character(length(tmp_state_transition))
      tmp_state_transition[row + j, 1]   <- block
      tmp_state_transition[row + j, 2]   <- age
      tmp_state_transition[row + j, 3]   <- ifelse(j %% 2 == 0, "Female", "Male")
      tmp_state_transition[row + j, 4]   <- initial_state
      tmp_state_transition[row + j, 5:8] <- "0"
      for (col in 1:length(to_states)) {
        tmp_state_transition[row + j, to_states[col]] <- as.character(states[col])
      }
    }
  }
}

state_transition <- age_correction(tmp_state_transition, age_bin_width)

## sort the state transition table to match the expected format for RESPOND input
## reference the output of `generate_shell_tables.R` when using the provided `user_inputs.R`
state_transition <- state_transition %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(initial_status, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")))

## BLOCK TRANS
## transitions between blocks - there is calibration data for both regular and post-treatments
## detox is the only treatment with time-varying targets for its transitions - isolate the multipliers
detox_mult            <- chosen_set[ grepl("detox_trans_mult", names(chosen_set)) ]

## create the input table for
tmp_block_transition      <- data.frame(matrix(nrow = 0, ncol = 22))
block_cols            <- c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox", "corresponding_post_trt")
colnames(tmp_block_transition) <- c("agegrp", "sex", "oud", "initial_block", block_cycles(block_cols, 52), block_cycles(block_cols, 104), block_cycles(block_cols, 156))
raw_block_transition <- raw_block_transition %>%
  arrange(agegrp, desc(sex), oud, factor(initial_block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")))

## separate the post-treatment transition rates from the rest of the data set (post-treatments are not age-stratified)
to_post               <- raw_block_transition %>% filter(startsWith(block_transitioned_to, "Post-"))
raw_block_transition  <- raw_block_transition %>% filter(!startsWith(block_transitioned_to, "Post-"))

## fill in the table
## because multiple lines of the calibration table are placed into a single
## row of the RESPOND input table, a placeholder index and while loop must be used.
raw_index             <- 1
while (raw_index < length(raw_block_transition[,1])) {
  row <- length(tmp_block_transition[,1])
  ## determine if the current state is a post-treatment or not, as the prescription varies
  is_post <- startsWith(raw_block_transition[raw_index, "initial_block"], "Post-")
  ## post-treatments are grouped as 5, while regular treatments are grouped as 4 - subtracting 1 since
  ## the current index is already the starting value
  grp <- ifelse(is_post, 4, 3)

  for (i in 1:2) {
    ## determine injection status, to break block transitions into injection and noninjection strata
    inject                   <- ifelse(i %% 2, "_Injection", "_Noninjection")
    ## create the row to store this data
    tmp_block_transition[row + i,]   <- character(length(tmp_block_transition))
    tmp_block_transition[row + i, 1] <- raw_block_transition[raw_index, "agegrp"]
    tmp_block_transition[row + i, 2] <- raw_block_transition[raw_index, "sex"]
    tmp_block_transition[row + i, 3] <- paste0(raw_block_transition[raw_index, "oud"], inject)
    tmp_block_transition[row + i, 4] <- raw_block_transition[raw_index, "initial_block"]
    ## initialize all values to zero, to account for those not filled from calibration data
    tmp_block_transition[row + i, 5:length(tmp_block_transition)] <- "0"
    ## assign values from the (fixed) calibration table to the data table, applying multipliers from calibration
    ## iterate over the group determined before, based on whether or not this is a post-treatment state
    for (j in raw_index:(raw_index + grp)) {
      ## select the current row of the fixed table
      demographic <- raw_block_transition[j,]
      ## the prefix of the column where this data will end up
      to_state    <- paste0("to_", demographic["block_transitioned_to"])
      probability <- cal_to_dist(as.character(demographic[6]),
                                 as.numeric(demographic[7]),
                                 as.numeric(demographic[8]))
      ## apply the multipliers to probability prior to adding them to the table
      ## recall, the only values that are multiplied are detox, as that is all that there are targets for
      for (k in 1:length(detox_mult)) {
        cycle       <- cycle_num_from_name(names(detox_mult[k]))
        if (to_state == "to_Detox") {
          tmp_block_transition[row + i, paste0(to_state, "_cycle", cycle)] <- probability * detox_mult[k]
        }
        else {
          tmp_block_transition[row + i, paste0(to_state, "_cycle", cycle)] <- probability
        }
      }
    }

    ## processing missing values - no treatment does not have a post-treament state, of course,
    ## so it has a different behavior than other blocks (to_corresponding_post_trt is always 0)

    ## define the column ranges for each time-dependent cycle - recall that data is split into `to_treatment_cycle#`
    ## columnar form
    time_dep <- seq(5, length(tmp_block_transition), by = length(block_cols))
    ## once again iterate over the detox multipliers to extract the identities of cycles
    for (j in 1:length(detox_mult)) {
      cycle       <- cycle_num_from_name(names(detox_mult[j]))
      ## the current state, needed to check whether or not this is no_treatment
      curr        <- tmp_block_transition[row + i, 4]
      if (!is_post & !(curr == "No_Treatment")) {
        ## post-transition is stratified by active or non-active use
        post_ind  <- ifelse((raw_block_transition[raw_index, "oud"] == "Active"), get_post_index(curr), get_post_index(curr) + 4)
        corresponding_post <- to_post[post_ind,]
        tmp_block_transition[row + i, paste0("to_corresponding_post_trt_cycle", cycle)] <-
          cal_to_dist(as.character(corresponding_post["distribution"]),
                      as.numeric(corresponding_post["param1"]),
                      as.numeric(corresponding_post["param2"]))

      }
      ## the sum of all present values in the row for the current time-dependence cycle
      ## all previously unspecified transition probaility is dumped into the current block
      remainder   <- sum(as.numeric(tmp_block_transition[row + i, time_dep[j]:(time_dep[j] + length(block_cols) - 1)]))
      if (remainder < 1) {
        if (is_post) {
          tmp_block_transition[row + i, paste0("to_corresponding_post_trt_cycle", cycle)] <- 1 - remainder
        }
        else {
          tmp_block_transition[row + i, paste0("to_", tmp_block_transition[row + i, 4], "_cycle", cycle)] <- 1 - remainder
        }
      }
    }
  }
  ## the +1 here accounts for the fact that `grp` was scaled down by 1 to point
  ## only to the indices within the same group
  raw_index   <- raw_index + grp + 1
}

block_transition <- age_correction(tmp_block_transition, age_bin_width)

block_transition      <- block_transition %>%
  arrange(agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")),
          factor(initial_block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")))

## ALL TYPES OVERDOSE
## overdoses
## define all treatments for use with post-treatments
treatments            <- c("Buprenorphine", "Naltrexone", "Methadone", "Detox")
## grab all calibrated overdose parameters
od_multipliers        <- chosen_set[ grepl("overdose_mult", names(chosen_set)) ]
## only grab the first 20 rows, as No_Treatment is the reference set
stratified_overdoses  <- raw_overdose[1:20, 2:5]
## grab the unique values of cycle from the column in the source calibration table
cycles <- as.numeric(unique(raw_overdose[,"time_varying_cycle"]))
for (cycle in 1:length(cycles)) {
  ## retrieve only the values related to the current cycle
  cycle_data <- raw_overdose[raw_overdose["time_varying_cycle"] == cycles[cycle],]["param1"]
  ## using "row" 1, because this is the No_Treatment case
  calibrated_cycle_data <- cycle_data * od_multipliers[1,"overdose_mult.No_Treatment"]
  stratified_overdoses[as.character(cycles[cycle])] <- calibrated_cycle_data
}
## create a placeholder of the No_Treatment case as a reference to be multiplied
## in future treatments
## all future treatments otherwise follow the same procedure of assignment and
## multiplication
reference             <- stratified_overdoses

## regular treatments
for (i in 2:(length(od_multipliers)-1)) {
  tmp <- reference
  treatment <- unlist(strsplit(names(od_multipliers[i]), "\\."))[2]
  tmp[,1] <- rep(treatment, 20)
  for (j in 5:length(colnames(reference))) {
    tmp[,j] <- tmp[,j] * od_multipliers[1,i]
  }
  stratified_overdoses <- rbind(stratified_overdoses, tmp)
}

## detox
## added separately because calibration
tmp <- reference
treatment <- "Detox"
tmp[,1] <- rep(treatment, 20)
for (j in 5:length(colnames(reference))) {
  tmp[,j] <- rep(0, 20)
}
stratified_overdoses <- rbind(stratified_overdoses, tmp)

## post-treatments
for (i in treatments) {
  tmp <- reference
  treatment <- paste0("Post-", i)
  tmp[,1] <- rep(treatment, 20)
  for (j in 5:length(colnames(reference))) {
    tmp[,j] <- tmp[,j] * od_multipliers[1,length(od_multipliers)]
  }
  stratified_overdoses <- rbind(stratified_overdoses, tmp)
}

colnames(stratified_overdoses) <- rename_columns("all_types_overdose_cycle", colnames(stratified_overdoses))

stratified_overdoses <- age_correction(stratified_overdoses, age_bin_width)
stratified_overdoses <- stratified_overdoses %>% arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")), agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection")))

## FATAL OVERDOSE
## fatal overdose proportions
## no elaborate processing necessary for these variables - retreive from the calibration
## data set and name the columns as they are in typical RESPOND input tables
fatal_overdose           <- chosen_set[ grepl("fatal_overdose", names(chosen_set)) ]
colnames(fatal_overdose) <- rename_columns("fatal_to_all_types_overdose_ratio_cycle", colnames(fatal_overdose))

### Writing RESPOND input tables

## STATIC TABLES
## tables in shared_data are not calibrated and do not need to be altered.
## copy these directly.
static_table_dir      <- "./calibration/data/fixed/shared_data/"
dir.create(in_name, showWarnings = FALSE)
## assigning to a value to suppress output
copied                <- file.copy(from = file.path(static_table_dir, list.files(static_table_dir)), to = in_name, recursive = TRUE)

## GENERATED TABLES
write.table(entering_cohort, file = paste0(in_name, "/entering_cohort.csv"), quote = FALSE, row.names = FALSE, sep = ',')
write.table(init_effect, file = paste0(in_name, "/block_init_effect.csv"), quote = FALSE, row.names = FALSE, sep = ',')
write.table(state_transition, file = paste0(in_name, "/oud_trans.csv"), quote = FALSE, row.names = FALSE, sep = ',')
write.table(block_transition, file = paste0(in_name, "/block_trans.csv"), quote = FALSE, row.names = FALSE, sep = ',')
write.table(stratified_overdoses, file = paste0(in_name, "/all_types_overdose.csv"), quote = FALSE, row.names = FALSE, sep = ',')
write.table(fatal_overdose, file = paste0(in_name, "/fatal_overdose.csv"), quote = FALSE, row.names = FALSE, sep = ',')

## write the seed out to a file so analysts can keep track of which rows of calibration data were used for which runs
seed_out              <- paste0("[", Sys.time(), "] SEED: ", format(chosen_set_num, width = 4), "; GENERATED FOLDER: ", in_name)
write(seed_out, file = "ec_seed", append = TRUE)
