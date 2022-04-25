infile <- function(s_id, filename) {
    ## a convenience function used to simplify the filename substitution below
    ## when the folder follows the format "inputX", where X is the strategy id
    ## e.g. strategy_id = 1, filename = "/init_cohort.csv" yields
    ##   "./input1/init_cohort.csv"
    return(paste("./input", s_id, filename, sep = ''))
}

## the names of the files containing the input tables
initial_cohort_file              <- infile(strategy_id, "/init_cohort.csv")
entering_cohort_file             <- infile(strategy_id, "/entering_cohort.csv")
oud_trans_file                   <- infile(strategy_id, "/oud_trans.csv")
block_trans_file                 <- infile(strategy_id, "/block_trans.csv")
block_init_effect_file           <- infile(strategy_id, "/block_init_effect.csv")
all_type_overdose_file           <- infile(strategy_id, "/all_types_overdose.csv")
fatal_overdose_file              <- infile(strategy_id, "/fatal_overdose.csv")
background_mortality_file        <- infile(strategy_id, "/background_mortality.csv")
SMR_file                         <- infile(strategy_id, "/SMR.csv")
healthcare_utilization_cost_file <- infile(strategy_id, "/cost/healthcare_utilization_cost.csv")
overdose_cost_file               <- infile(strategy_id, "/cost/overdose_cost.csv")
treatment_utilization_cost_file  <- infile(strategy_id, "/cost/treatment_utilization_cost.csv")
pharmaceutical_cost_file         <- infile(strategy_id, "/cost/pharmaceutical_cost.csv")
background_utility_file          <- infile(strategy_id, "/cost/bg_utility.csv")
oud_utility_file                 <- infile(strategy_id, "/cost/oud_utility.csv")
setting_utility_file             <- infile(strategy_id, "/cost/setting_utility.csv")
