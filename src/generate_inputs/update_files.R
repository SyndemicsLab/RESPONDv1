# This files accept an integer as its input which indicates the row id of the symbol table.

# load required libraries and read input files
args <- commandArgs(trailingOnly=TRUE)
row_id <- as.numeric(args[1])

# open a file to sink the errors
msgcon <- file(paste("update_files_errors",row_id,".txt",sep=""), open = "w")
sink(msgcon , append = FALSE, type = c("message"), split = FALSE)
     
source("src/generate_inputs/update_files_funcs.R")
symbol_info <- read.csv("./shared_data/symbols_info.csv")
multipliers <- as.matrix(read.csv("./shared_data/symbol_tbl.csv"))[row_id,]
# --------------------------------------------------------------------------------------------------------------------
# create symbol_files table
symbols <- vector(mode = "character")
files <- vector(mode = "character")
for (i in 1:nrow(symbol_info))
{
  tmp_symbols <- unlist(strsplit(as.character(symbol_info$symbols[i]),"#"))
  tmp_files <- rep(as.character(symbol_info$input_filename[i]),length(tmp_symbols))

  symbols <- c(symbols,tmp_symbols)
  files <- c(files,tmp_files)
}
symbol_files <- rbind(symbols,files)
# --------------------------------------------------------------------------------------------------------------------
files <- unique(symbol_files[2,])
# call corresponding update functions and pass symbols and multipliers  
for (i in 1:length(files))
{
  symbol_input <- symbol_files[1,which(symbol_files[2,] == files[i])]
  mult_input <- multipliers[symbol_input]
  input_folders <- list.files(path = ".", pattern = "input", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE)
  if (length(grep("block_trans.csv",files[i])) != 0)
  {
    if (tolower(symbol_info$shared_status[which(symbol_info$input_filename == "block_trans.csv")]) == "yes")
    {
      file_path <- "./shared_data/block_trans.csv"
        update_blk_trans(symbol_input,mult_input,file_path)
    } else {
      for (ii in 1:length(input_folders))
      {
        file_path <- paste("./input",ii,"/block_trans.csv",sep="")
        update_blk_trans(symbol_input,mult_input,file_path)
      }
    }
  } else if (length(grep("fatal_overdose.csv",files[i])) != 0)
  {
    if (tolower(symbol_info$shared_status[which(symbol_info$input_filename == "fatal_overdose.csv")]) == "yes")
    {
      file_path <- "./shared_data/fatal_overdose.csv"
      update_fatal_od(symbol_input,mult_input,file_path)
    } else {
      for (ii in 1:length(input_folders))
      {
        file_path <- paste("./input",ii,"/fatal_overdose.csv",sep="")
        update_fatal_od(symbol_input,mult_input,file_path)
      }
    }
  }
}
