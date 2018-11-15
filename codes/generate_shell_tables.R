# Generate input shell tables.

# Set your current working directory here, if required.
# No need to set working directory if used as a project.
# all codes and inputs should be inside this folder in their specific format, name and path.
#setwd("~/workspace/RESPOND")

# load general user inputs
source("inputs/user_inputs.R")
# check the general user inputs
source("codes/generate_inputs/check_general_inputs.R")

# Open a connection to write warning and error messages
msgcon <- file(paste("errors",run_id,".txt",sep=""), open = "w")
sink(msgcon , append = FALSE, type = c("message"),
     split = FALSE)

# Check general inputs
check_general_inputs()

# If there is any warning in user inputs, stop and print the warning messages.
if (length(warnings()) != 0)
{
  quit(save="no")
}

source("codes/generate_inputs/generate_deterministic_input_shell_tables.R")
generate_deterministic_input_shell_tables()

if (input_type == "stochastic")
{
  source("codes/generate_inputs/generate_stochastic_input_shell_tables.R")
  generate_stochastic_input_shell_tables()
}


