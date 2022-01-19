### SUMMARY OF SALES ####

# define constant inputs
# expected columns order and names to be assigned
columns <- c("store_name", "store_number", "count_Azalea", "rev_Azalea", 
             "count_Begonia", "rev_Begonia", "count_Carnation", "rev_Carnation",
             "count_Daffodil", "rev_Daffodil", "count_total", "rev_total")
# TODO data types


add_month_year <- function(file_name, df){
  '
  Function which extracts month and year from file name (the only place where we have this info)
  Extracts Store ID information from store_number column (last 3 digits of long numbers)
  It adds this information as columns to identify the data and be able to join it with other data
  The function also splits data into used part (totals) and part which is not of primary focus for us (rest)
  THINK - The last step could be moved to separate function
  inputs:
    -> file_name (string) - relative path to processed file
    -> df (data.frame) - data.frame obtained by reading in the file
  output:
    <- ls_df (list) - the list of 2 data.frames: totals (used data) and rest (GROSS / RETAIL etc.)
  '
  # In this step we extract the month and the year - they should appear after Sales phrase
  month_year <- stringr::str_match(file_name, "Sales\\s*(.*?)\\s*.csv")[,2]
  # splitted by space 
  month_year <- strsplit(month_year, split=' ')[[1]]
  
  # month is the first element and year is the second one
  mnth <- match(month_year[1], month.name)
  yr <- as.numeric(month_year[2]) 
  
  # adding them as columns of integer data type
  df <- df %>% mutate(month_id = mnth, year_id = yr)
  
  # Data.frame z GROSS / RETAIL / OTHER 
  # maybe we will use it later 
  df_scam <- df %>% filter(!str_length(as.character(store_number)) == 10)
  
  # Filter only the data we need (totals)
  df <- df %>% filter(str_length(as.character(store_number)) == 10) %>% 
    mutate(store_id = as.numeric(substr(as.character(store_number),8, 10)))
  
  # R doesn't support returning multiple objects - returns list
  ls_df = list("totals" = df, "rest" = df_scam)
  return(ls_df)
}


union_sales_data <- function(file_paths){
  '
  Function which loops over the list of "Summary of Sales ...csv" files and applies transformations by add_month_year() 
  It creates a list of data.frames (one for each month - as files)and binds them together later
  If instructions change the function can easily be adpated to return also data about GROSS / RETAIL etc. sales
  inputs:
    -> file_paths (string) - relative path to processed file
  output:
    <- df_totals (data.frame) - the list of 2 data.frames: totals (used data) and rest (GROSS / RETAIL etc.)
  '
  # initialize lists of data.frames and a pointer
  sales_dflist <- list()
  discarded_dflist<- list() # this one has GROSS / RETAIL / OTHER data which we are not interested in now
  i <- 1 
  
  for (file in file_paths)
    {
    # read data.frame with monthly data
    df_mth <- read_delim(file, delim = ",", col_names = columns)
    print(paste("wczytano plik", file))
    
    # Add information about month year and extracted store ID (this is necessary for later join)
    ls_trans_df <- add_month_year(file, df_mth)
    
    # append this data.frame as element of the list which will be binded later
    sales_dflist[[i]] <- ls_trans_df$totals
    discarded_dflist[[i]] <- ls_trans_df$rest
    # increase the pointer
    i <- i + 1
    }
  
  # make a UNION - bind list of data.frames together
  df_sales <- dplyr::bind_rows(sales_dflist)
  df_rest <- dplyr::bind_rows(discarded_dflist)
  ls_df <- list("df_totals" = df_sales, "df_rest" = df_rest)
  
  # For now returning only the totals object - may change behaviour in the future if other data will be required
  return(ls_df$df_totals)
}
