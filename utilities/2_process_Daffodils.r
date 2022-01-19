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
  ###Inputs:
  ###path - path of a excel Parviflora Report file
  ###sheet_name - name of a sheet from which the table is to be retrieved
  '
  
  #creating excel object
  df <- read_excel(path, sheet=sheet_name)
  
  #filtering out unnecessary data and renaming columns, removing rows with NAs in labels column 
  df <- df %>% rename(labels=1, trans_amount=4, trans_count=5) %>% select(1,4,5) %>% filter(!is.na(labels))
  
  #defining the values of the beginning cell and ending cell of summary table
  first_cell <- 'Summary for Period'
  last_cell <- 'Summary Totals'
  
  
  #retrieving the coordinates of the first row label and last row labels of a summary_for_period table
  beginning_row <- stack(setNames(lapply(df, grep, pattern = first_cell), 'labels'))[[1]]
  end_row <- stack(setNames(lapply(df, grep, pattern = last_cell), 'labels'))[[1]]
  
  #selecting only rows that lie between beginning row and end_row, which are start and end of the summary_for_period table
  df <- df %>% slice((beginning_row+1):end_row)
  
  #converting value columns to numeric format and changing null values to 0
  df <- df %>% mutate(trans_count=as.numeric(trans_count)) %>% mutate(trans_amount=as.numeric(trans_amount)) %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  return(df)
}


parse_xls_table <- function(path, sheet, pivot=TRUE){
  '
  #TODO: remove -- in values 
  #This function takes Parviflora Financial Report (part with each shops report) and bundles it together in one table
  #inputs:
  #path - path of a excel file
  #sheet - name or index of a excel sheet
  #pivot (TRUE/FALSE), default:TRUE - if TRUE function returns pivoted table (if no other major transformations are to be done), if FALSE function returns unpivoted table (if other transformations are to be applied to it later on, i.e. merging with other months)
  '
  
  # TODO rio package gives the option to read the whole file and store read-in sheets as data.frames inside a list
  #Reading in the data
  df <- read_excel(path, sheet = sheet) # Equivalent
  df <- df %>% rename(labels=1, val1=2, val2=3, val3=4, val4=5)
  
  #setting the value of a first label cell in each individual report to later search for coordinates
  first_cell <- 'Submitting Location:'
  
  #creating a vector with locations of each row containing first_cell value
  beginning_row <- stack(setNames(lapply(df, grep, pattern = first_cell), 'labels'))[[1]]
  
  #removing rows with summary table
  df <- df %>% slice(beginning_row[1]:nrow(df)) %>% select(-c(4,5))
  
  #creating helper table with all rows labels and row and col indexes
  labels <- df %>% filter(!is.na(labels))
  
  
  #creating a vector with locations of each row containing first_cell value
  #there is one uneeded row in each shop's table, which is
  #submitting location
  #code below removes it
  
  #creating a vector with locations of each row containing first_cell value
  loc <- stack(setNames(lapply(labels, grep, pattern = first_cell), 'labels'))[[1]]
  
  #removing unneeded rows based on the locations calculated above 
  labels_2 <- labels %>% slice(-c(loc))
  
  df <- labels_2 %>% rename(trans_amount=2, trans_count=3)
  
  #extracting ids and codes
  additional_col_names <- labels %>% slice(c(loc)) %>% rename(id=2, code=3) %>% select(-1)
  
  #putting ids in rows where labels == 'Totals' beceause this is the value that is exatcly the same in each table and is there always
  value_to_search_for <- 'Totals'
  intervals <- stack(setNames(lapply(df, grep, pattern = value_to_search_for), 'labels'))[[1]]
  
  #creating empty id and code column to later fill them in with corresponding data
  df$id <- NA
  df$code <- NA
  
  #looping through all rows with 'Total' in labels column and putting a corresponding id and code in respective columns
  for (i in 1:length(intervals)) {
    df[intervals[i],'id'] <- additional_col_names$id[i]
    df[intervals[i], 'code'] <- additional_col_names$code[i]
  }
  
  #filling empty rows in id columns with ids 
  df <- df %>% fill(id, .direction='up') #%>% filter(!character=='--')
  df <- df %>% fill(code, .direction='up')
  
  #There is a situation where labels duplicate, in those instances value for one label is neagative, because all labels in each table need to be unique,
  #in the code below sufixes are added to labels with negative values
  df_trimmed <- df %>%
    mutate(trans_amount = as.numeric(trans_amount), trans_count=as.numeric(trans_count)) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(label_suffix = case_when(trans_amount<0 ~ ' (neg)', TRUE ~ '')) %>%
    mutate(labels = paste(labels, label_suffix)) %>% select(-label_suffix)
  
  #To make the final table there needs to be only one value column, therefore trans_amount and trans_count are going to be merged
  # and additional label column is going to be created
  df_trans_amount <- df_trimmed %>% select(labels, trans_amount, id, code)
  df_trans_count <- df_trimmed %>% select(labels, trans_count, id, code)
  
  df_trans_amount <- df_trans_amount %>% rename(value=trans_amount)
  df_trans_count <- df_trans_count %>% rename(value=trans_count)
  
  df_trans_amount$position <- 'trans_amount'
  df_trans_count$position <- 'trans_count'
  
  #creating a stacked tables
  df_final <- rbind(df_trans_amount, df_trans_count)
  
  #adding month column
  # For the purpose of doing analysis and joining data.frames 
  # it would be perhaps better to have separate columns for month and year
  month_number <- match(substr(sheet,1,3),month.abb)
  
  # For now month column is in format "Feb20"
  df_final$month <- sheet
  
  #if pivot argument is true, the function pivots df_final and returns the pivoted frame, else it return just df_final - this can be used if later on all months are to be combined in one table
  if (pivot==TRUE) {
    
    #pivoting the final table
    df_final_pivot <- spread(df_final, labels, value)
    df_final_pivot <- df_final_pivot %>% select(-names(df_final_pivot)[16])
    
    return(df_final_pivot)
    
  } else {
    
    df_final <- df_final %>% slice(-nrow(df_final))
    return(df_final)
    
  }
}


merge_summaries <- function(xlsx_path){
  '
  #this function creates one summary-for_period combining each individual summary from one sheet of the xls file
  #input
  #xlsx_path - path to xlsx file
  '
  
  #extracting names of sheets (months)
  sheets <- excel_sheets(xlsx_path)
  
  #creating an empty table to fill with data using for loop
  df <- data.frame(position=c(), trans_amount=c(), trans_count=c())
  
  #looping through sheets and creating summaries, then binding them together
  for (s in sheets){
    df_1 <- parse_period_summary(xlsx_path, s)
    df_1$month <- s
    df <- rbind(df, df_1)
  }
  
  #changing names of duplicate labels with negative values by adding suffix (neg)
  df_trimmed <- df %>%
    mutate(label_suffix = case_when(trans_amount<0 ~ ' (neg)', TRUE ~ '')) %>%
    mutate(labels = paste(labels, label_suffix)) %>% select(-label_suffix)
  
  #separating df to two with separate values in each to later combine to one df with only one value column
  df_trans_amount <- df_trimmed %>% select(-trans_count) %>% mutate(value_label='trans_amount') %>% rename(values=trans_amount)
  df_trans_count <- df_trimmed %>% select(-trans_amount) %>% mutate(value_label='trans_count') %>% rename(values=trans_count)
  
  #binding the two df together and spreading the df to simplify it
  df <- rbind(df_trans_amount, df_trans_count) %>% spread(labels, values)
  
  return(df)
}


combine_tables <- function(path){
  
  '
  Function loops through all sheets in xls file and creates tabels, then binds them togheter and pivots into one final
  Inputs:
  path - path to excel file
  '
  
  #extracting sheet names
  sheets <- excel_sheets(path)
  
  #creating empty dataframe to later fill in
  df <- data.frame(labels=c(), value=c(), id=c(), code=c(), position=c(), month=c())
  
  #looping through all sheets in the excel file, creating tabels with financial data and binding them all together 
  for (s in sheets) {
    df_1 <- parse_xls_table(path, s, pivot=FALSE)
    df <- rbind(df, df_1)
  }
  
  #pivoting the table
  df_pivoted <- df %>% spread(labels, value)
  
  return(df_pivoted)
}




