# Generate input shell tables.

# Set your current working directory here, if required.
# No need to set working directory if used as a project.
# all codes and inputs should be inside this folder in their specific format, name and path.
#setwd("~/workspace/RESPOND")

msgcon <- file(paste("generate_deterministic_shell_tables_errors.txt",sep=""), open = "w")
sink(msgcon , append = FALSE, type = c("message"), split = FALSE)

# load general user inputs
source("inputs/user_inputs.R")
# check the general user inputs
source("codes/generate_inputs/check_general_inputs.R")

# Check general inputs
check_general_inputs()

source("codes/generate_inputs/generate_deterministic_input_shell_tables.R")
generate_deterministic_input_shell_tables()



