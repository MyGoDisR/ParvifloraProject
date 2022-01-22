#library(readxl)
#library(tidyverse)

#FUNCTIONS:
##################################################################################################################################################################################

get_file_paths <- function(pattern = "Daffodils*.xls"){
# I guess this one is deprecated with the use of list.files()
  '
    This function dynamically creates paths to files matching given pattern.
    Intended to use to find all Dafodils sales .xls files in the working directory
    input:
      pattern (str) - pattern to find (default = "Daffodils*.xls")
    returns:
      vector of absolute file paths
  '
  # Create a list of all paths to Daffodils files
  daffodils_file_paths <- c()
  
  for(f in Sys.glob(pattern)){
    daffodils_file_paths <- c(daffodils_file_paths,
                              paste(normalizePath(dirname(f)), fsep = .Platform$file.sep, f, sep = "")) 
  }
  return(daffodils_file_paths)
}




parse_period_summary <- function(path, sheet_name){
  '
  function retrieves and formats summary_for_period table from Parviflora Financial Report Excel File
  Inputs:
    -> path - path of a excel Parviflora Report file
    -> sheet_name - name of a sheet from which the table is to be retrieved
  Output:
    <- df - data.frame object with summary for period for one month
  '
  
  #creating excel object
  df <- readxl::read_excel(path, sheet=sheet_name)
  
  #filtering out unnecessary data and renaming columns, removing rows with NAs in labels column 
  df <- df %>%
    dplyr::rename(labels=1, trans_amount=4, trans_count=5) %>%
    dplyr::select(1,4,5) %>%
    dplyr::filter(!is.na(labels))
  
  #defining the values of the beginning cell and ending cell of summary table
  first_cell <- 'Summary for Period'
  last_cell <- 'Summary Totals'
  
  
  #retrieving the coordinates of the first row label and last row labels of a summary_for_period table
  beginning_row <- stack(setNames(lapply(df, grep, pattern = first_cell), 'labels'))[[1]]
  end_row <- stack(setNames(lapply(df, grep, pattern = last_cell), 'labels'))[[1]]
  
  #selecting only rows that lie between beginning row and end_row, which are start and end of the summary_for_period table
  df <- df %>% dplyr::slice((beginning_row+1):end_row)
  
  #converting value columns to numeric format and changing null values to 0
  df <- df %>%
    dplyr::mutate(trans_count=as.numeric(trans_count)) %>%
    dplyr::mutate(trans_amount=as.numeric(trans_amount)) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  return(df)
}




parse_xls_table <- function(path, sheet, totals_only=TRUE) {
  '
  This function takes Parviflora Financial Report (part with each shops report) and bundles it together in one table,
  performing neccessary transformations and additions along the way
  
  inputs:
    -> path - relative path of a excel file
    -> sheet - name or index of a excel sheet from which the data is to be extracted
    -> totals_only (TRUE/FALSE) -  default:TRUE - if TRUE function returns only rows with label Totals
                                   if FALSE function returns all rows
  outputs:
    <- df - data.frame object with all Financial Report positions or just totals (depending on the totals_only argument) in a "tidy" format 
  '
  
  #Reading in the data
  df <- readxl::read_excel(path, sheet = sheet) # Equivalent
  df <- df %>% dplyr::rename(labels = 1, val1 = 2, val2 = 3, val3 = 4, val4 = 5)
  
  #Setting the value of a first label cell in each individual report to later search for coordinates
  first_cell <- 'Submitting Location:'
  
  #Creating a vector with locations of each row containing first_cell value
  start_row <- stack(setNames(lapply(df, grep, pattern = first_cell), 'labels'))[[1]]
  
  #Removing rows with summary table
  df <- df %>% dplyr::slice(start_row[1]:nrow(df)) %>% select(-c(4,5))
  
  #Creating helper table with all rows labels and row and col indexes
  labels <- df %>% dplyr::filter(!is.na(labels))
  
  
  #Creating a vector with locations of each row containing first_cell value, although
  #there is one uneeded row in each shop's table, which is submitting location
  
  #Creating a vector with locations of each row containing first_cell value
  loc <- stack(setNames(lapply(labels, grep, pattern = first_cell), 'labels'))[[1]]
  
  #Removing unneeded rows based on the locations calculated above 
  labels_final <- labels %>% dplyr::slice(-c(loc))
  
  #Creating table with trans_amount and trans_count columns
  df <- labels_final %>% dplyr::rename(trans_amount = 2, trans_count = 3 )
  
  #Extracting ids and codes
  id_code <- labels %>%
    dplyr::slice(c(loc)) %>%
    dplyr::rename(id = 2, store_id = 3) %>%
    dplyr::select(-1)
  
  #Putting ids in rows where labels == 'Totals' beceause this is the value that is exatcly the same in each table and is there always
  search_char <- 'Totals'
  intervals <- stack(setNames(lapply(df, grep, pattern = search_char), 'labels'))[[1]]
  
  #Creating empty id and code column to later fill them in with corresponding data
  df$id <- NA
  df$store_id <- NA
  
  #Looping through all rows with 'Total' in labels column and putting a corresponding id and code in respective columns
  for (i in 1:length(intervals)) {
    df[intervals[i],'id'] <- id_code$id[i]
    df[intervals[i], 'store_id'] <- id_code$store_id[i]
  }
  
  #Filling empty rows in id and code columns with ids and codes corresponding to them
  df <- df %>% tidyr::fill(c(id,store_id), .direction = 'up') #%>% filter(!character=='--')
  
  
  #There is a situation where labels duplicate, in those instances value for one label is neagative, because all labels in each table need to be unique,
  #in the code below sufixes are added to labels with negative values and also neccessary tidying is applied
  df <- df %>%
    dplyr::mutate(trans_amount = as.numeric(trans_amount), trans_count = as.numeric(trans_count)) %>% #converting value columns to numeric format
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #replacing every NA in numeric column with 0
    dplyr::mutate(label_suffix = case_when(trans_amount<0 ~ ' (neg)', TRUE ~ '')) %>% #creating suffix to duplicate labels with neg tot_amnt
    dplyr::mutate(labels = paste(labels, label_suffix)) %>% # inserting suffix to labels
    dplyr::select(-label_suffix) %>% #deleting suffix column
    dplyr::mutate(labels = trimws(labels, which = c('both'))) %>% #removing trailing and leading spaces from labels
    tidyr::drop_na(id) %>% #dropping rows where there is no id
    dplyr::mutate(store_id=as.numeric(store_id)) #converting store_id to numeric format
  
  
  #To make the final table there needs to be only one value column, therefore trans_amount and trans_count are going to be merged
  #and additional label column is going to be created 
  df<- df %>% tidyr::pivot_longer(trans_amount:trans_count, names_to = 'position', values_to = 'value')
  
  #Adding month and year column extracted from the sheet name
  #Extracting year from the file name
  df$month <- sheet %>% stringr::str_sub(1,3) %>% match(month.abb)
  df$year <- stringr::str_match(path, "Daffodils\\s*(.*?)\\s*.xls$")[,2] %>% as.numeric()
  
  
  if (totals_only==TRUE) {
    
    df <- df %>% dplyr::filter(labels == 'Totals')
    
    return(df)
    
  } else {
    
    return(df)
    
  }
  
  #MESSAGE: Decided to delete the PIVOT functionality, since with totals_only argument it would produce too complicated code imo, moreover the pivoted table is not needed for our analysis
  #although if in the future it would be needed it should be applied in a separate function.
  
  # #If pivot argument is true, the function pivots df_final and returns the pivoted frame, else it return just df_final - this can be used if later on all months are to be combined in one table
  # if (pivot == TRUE) {
  #   
  #   #Pivoting the final table
  #   df_final_pivot <- tidyr::spread(df, labels, value)
  #   df_final_pivot <- df_final_pivot %>% dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  #   
  #   return(df_final_pivot)
  #   
  # } else {
  #     
  #   return(df)
  #   
  # }
}



merge_summaries <- function(path, totals_only=TRUE){
  '
  this function creates one summary-for_period combining each individual summary from one sheet of the xls file
  Inputs:
    -> xlsx_path - path to xlsx file
    -> totals_only (TRUE/FALSE) - if TRUE returns only rows corresponding to "Summary Totals" label, if FALSE returns all rows
  Output:
    <- df - data.frame with summary for period table for all months
  '
  
  #extracting names of sheets (months)
  sheets <- readxl::excel_sheets(path)
  
  #creating an empty table to fill with data using for loop
  df <- data.frame(position=c(), trans_amount=c(), trans_count=c())
  
  #looping through sheets and creating summaries, then binding them together
  for (s in sheets){
    df_1 <- parse_period_summary(path, s)
    df_1$month <- s
    df <- rbind(df, df_1)
  }
  
  #changing names of duplicate labels with negative values by adding suffix (neg)
  df_trimmed <- df %>%
    dplyr::mutate(label_suffix = case_when(trans_amount<0 ~ ' (neg)', TRUE ~ '')) %>%
    dplyr::mutate(labels = paste(labels, label_suffix)) %>% select(-label_suffix)
  
  #putting trans_count and trans_amount into one value column and 
  df <- df %>% tidyr::pivot_longer(trans_amount:trans_count, names_to = 'value_label', values_to = 'values')
  
  if (totals_only==TRUE) {
    
    df <- df %>% dplyr::filter(labels=='Summary Totals')
    return(df)
    
  } else {
    
    return(df)  
    
  }
  
}



combine_tables <- function(path, totals_only=TRUE){
  
  '
  Function loops through all sheets in xls file and creates tabels, then binds them togheter and pivots into one final
  Inputs:
    -> path - path to excel file
    -> totals_only (TRUE/FALSE) - if TRUE returns object with only Totals, if FALSE returns object with all Financial Report positions
  Output:
    <- df - data.frame object with totals for Daffodils for each month in a "tidy" format
  '

  #Extracting sheet names
  sheets <- readxl::excel_sheets(path)
  
  #Creating empty dataframe to later fill in
  df <- data.frame(labels=c(), value=c(), id=c(), code=c(), position=c(), month=c())
  
  #Looping through all sheets in the excel file, creating tabels with financial data and binding them all together 
  for (s in sheets) {
    df_1 <- parse_xls_table(path, s, totals_only=totals_only)
    df <- rbind(df, df_1)
  }
  
  # Discuss: This function may return different results for totals and all fields now... 
  #     it is all hypothetical as for this project we don't need to return anythin other than totals
  
  #pivoting the table and replacing NAs in value columns (if there are any) with 0s
  if(totals_only == TRUE){
    # adding this pivot here for easier joins on TOTALS  
    df_pivoted <- df %>% select(-labels) %>% 
      tidyr::pivot_wider(names_from = position, values_from = value)  %>%
      dplyr::mutate_at(vars('trans_amount', 'trans_count'), ~replace(., is.na(.), 0))
  
    } else {
    # if we want all values not only totals then we return the object in the long form 
    # BUT: to be honest I would leave the labels as the column and return pivoted version anyway
      # example: if we want to return sum of "Small Business" revenue - having this info in one column "labels"
      # would allow us to simply group by this column without filtering "position column" first
      # in the end it depends what are we going to do with this data...
    
    df_pivoted <- df %>%
      tidyr::spread(labels, value) %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  }
  
  
  return(df_pivoted)
}



