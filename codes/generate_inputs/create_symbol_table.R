# This file reads the symbol info table and creates a new symbol table which includes all permutation of all symbols.
symbol_info <- read.csv("./shared_data/symbols_info.csv")
symbols <- vector(mode = "character")
symbols_ranges <- vector(mode = "character")
for (i in 1:nrow(symbol_info))
{
  symbols <- c(symbols,unlist(strsplit(as.character(symbol_info$symbols[i]),"#"))) 
  symbols_ranges <- c(symbols_ranges,unlist(strsplit(as.character(symbol_info$symbol_ranges[i]),"#")))
}

if (length(symbols) != length(symbols_ranges))
{
  stop("Each symbol should have a corresponding range!")
}

range_list <- list()
for (i in 1:length(symbols))
{
  tmp_range <- as.numeric(unlist(strsplit(symbols_ranges[i],",")))
  range_list[[i]] <- seq(tmp_range[1],tmp_range[2],tmp_range[3])
}

symbol_tbl <- expand.grid(range_list)
write.table(symbol_tbl,"./shared_data/symbol_tbl.csv",col.names = symbols, row.names = FALSE, sep=",")
