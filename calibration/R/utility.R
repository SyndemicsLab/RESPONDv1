##' read_params
##'
##' Returns the csv table as a list object, ignoring whether
##' or not the column name is a valid variable argument
##' @title read_params
##' @param table_name a csv file path
##' @return The list object containing all of the values in the
##'   source csv file
read_params <- function(table_name) {
  return(read.csv(table_name, check.names = FALSE))
}


##' cycle_num_from_name
##'
##' Extracts the time-dependent cycle values from column names
##' provided in fixed parameter tables.
##'
##' @title cycle_num_from_name
##' @param c_name
##' @return The cycle number in the string \code{c_name}
cycle_num_from_name <- function(c_name) {
  ## given full source column names, extract and return the cycle for time-dependence
  name_pos <- gregexpr('\\d+', c_name)
  cycle_num <- substr(c_name, name_pos[[1]], name_pos[[1]] + attr(name_pos[[1]], "match.length"))
  return(cycle_num)
}


process_entering <- function(entering_cohort, calibrated, c_names) {
  ## takes in the fixed parameters table, calibrated multiplier parameters, and the associated column names
  ## for the calibrated multipliers then constructs the RESPOND input table

  ## copy columns common between calibration and the model inputs
  to_return <- entering_cohort[1:10,2:3]
  for (cycle in 1:length(calibrated)) {
    ## determine the cycle number associated with the transition
    cycle_num <- cycle_num_from_name(c_names[cycle])
    ## grab all fixed data associated with that cycle number and scale it by the calibration parameter
    to_return[cycle_num] <-
      entering_cohort[entering_cohort["time_varying_cycle"] == as.numeric(cycle_num),"param1"] * calibrated[,cycle]
  }
  colnames(to_return) <- rename_columns("number_of_new_comers_cycle", colnames(to_return))
  return(to_return)
}


rename_columns <- function(template, col_names) {
  to_return <- character()
  for (i in 1:length(col_names)) {
    cycle_num <- cycle_num_from_name(col_names[i])
    if (cycle_num != '') {
      to_return[length(to_return) + 1] <- paste(template, cycle_num, sep = '')
    }
    else {
      to_return[length(to_return) + 1] <- col_names[i]
    }
  }
  return(to_return)
}


oud_state_col <- function(initial_state, to_states) {
  forbidden_state <- switch(initial_state,
                            "Active_Injection" = "to_Nonactive_Noninjection",
                            "Active_Noninjection" = "to_Nonactive_Injection",
                            "Nonactive_Injection" = "to_Nonactive_Noninjection",
                            "Nonactive_Noninjection" = "to_Nonactive_Injection"
                            )
  return(setdiff(c("to_Active_Noninjection", "to_Active_Injection", "to_Nonactive_Noninjection", "to_Nonactive_Injection"), c(to_states, forbidden_state)))
}


cal_to_dist <- function(dist, a, b) {
  dist_func <- switch(dist, "norm" = rnorm, "unif" = runif)
  return(dist_func(1, a, b))
}


block_cycles <- function(ref, cyc) {
  to_return <- character()
  for (val in ref) {
    to_return <- c(to_return, paste0("to_", val, "_cycle", cyc))
  }
  return(to_return)
}


get_post_index <- function(initial_state) {
  return(switch(initial_state,"Buprenorphine" = 1, "Naltrexone" = 2, "Methadone" = 3, "Detox" = 4))
}


age_correction <- function(tmp_table, age_bin_width) {
  to_return <- data.frame(matrix(nrow = 0, ncol = length(tmp_table)))
  colnames(to_return) <- colnames(tmp_table)
  for (i in 1:length(tmp_table[,1])) {
    curr <- tmp_table[i,]
    ages <- unlist(strsplit(curr[,"agegrp"], split = '_'))
    group_bounds <- as.numeric(ages)
    new_ages <- seq(group_bounds[1], group_bounds[2], by = age_bin_width)
    for (lower in new_ages) {
      row <- length(to_return[,1]) + 1
      to_return[row,] <- character(length(to_return))
      to_return[row, "agegrp"] <- paste0(lower,'_',lower+age_bin_width-1)
      for (val in colnames(curr)[!colnames(curr) == "agegrp"]) {
        to_return[row, val] <- curr[,val]
      }
    }
  }
  return(to_return)
}
