# This file includes the functions that update the input tables.
# Each functions requires 3 input arguments: 1-symbols, 2-multipliers, 3-file shared status

if ("stringr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages("stringr",repos = "http://cran.us.r-project.org")
} else {
  library(stringr)
}

# update_block_trans
update_blk_trans <- function (symbols,mult,file)
{
  df <- read.csv(file)
  # calculate number of co  lumns per time interval
  tmp_cycle <- names(df)[5]
  tmp_cycle <- unlist(strsplit(tmp_cycle,"_"))
  tmp_cycle <- tmp_cycle[length(tmp_cycle)]
  num_col <- length(grep(tmp_cycle,names(df)))
  num_intervals <- (ncol(df)-4)/num_col
  
  for (c in 1:num_intervals)
  {
    tmp_df <- as.matrix(df[,(4*c+1):(4*c+num_col)])    # seprate each time interval
    for (i in 1:nrow(tmp_df))   # read data row by row
    {
      row_values <- tmp_df[i,]
      s_flag <- FALSE
      for (j in 1:length(symbols))
      {
        tmp <- grep(symbols[j],row_values)
        if (length(tmp) > 0)
        {
          for (ii in 1:length(tmp))
          {
            tmp_value <- as.numeric(str_remove(row_values[tmp[ii]],symbols[j]))
            row_values[tmp[ii]] <- 1-exp(log(1-tmp_value)*mult[j])
          }
          s_flag <- TRUE
        }
      }
      if (s_flag)
      {
        tmp <- grep("S",row_values)
        if (length(tmp) != 1)
          warning(paste("Missing or multiple -S- values in ",file, sep=""))
        row_values_tmp <- row_values
        row_values[tmp] <- 1-sum(as.numeric(row_values_tmp[-tmp]))
        if (as.numeric(row_values[tmp]) < 0)
        {
          warning(paste("Invalid block transition values!",i,sep = "_"))
        }
      }
      tmp_df[i,] <- row_values
    }
    df[,(4*c+1):(4*c+num_col)] <- tmp_df
  }
  if (length(warnings()) == 0)
  {
    write.csv(df,file,row.names = FALSE,quote = FALSE)
  }
}
# -------------------------------------------------------------------------------------------------------------------
# update_fatal_od
update_fatal_od <- function (symbols,mult,file)
{
  df <- as.matrix(read.csv(file))
  for (j in 1:length(symbols))
  {
    tmp <- grep(symbols[j],df)
    if (length(tmp) > 0)
    {
      for (ii in 1:length(tmp))
      {
        df[tmp[ii]] <- as.numeric(str_remove(df[tmp[ii]],symbols[j]))*mult[j]
      }
    }
  }
  if (range(as.numeric(df))[1] < 0 | range(as.numeric(df))[2] > 1)
  {
     warning("Invalid fatal overdose values!")         
  }
  if (length(warnings()) == 0)
  {
    write.csv(df,file,row.names = FALSE,quote = FALSE)
  }
}
# -------------------------------------------------------------------------------------------------------------------