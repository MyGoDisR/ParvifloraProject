library(readxl)
library(tidyverse)
library(tidyxl)

xlsx_path <- '/Users/Maciek/Desktop/Master_Studies/Itroduction_to_programming_with_R/R_Final_Project/R_proj/Daffodils2020.xlsx'
sheet_name <- 'Feb20'

###TODO: add store id and month
### convert xls to xlsx
### looping through all sheets

cells <- xlsx_cells(xlsx_path)
get_sheet_names <- function(path){
  #This function returns names of all the sheets in the excel files
  #inputs:
  #path - path of a xlsx file
  
  cells <- xlsx_cells(path)
  
  return(unique(cells$sheet))
}

summary_for_period <- function(path, sheet_name){
  ##This function retrieves and formats summary_for_period table from Parviflora Financial Report Excel File
  ###Inputs:
  ###path - path of a xlsx Parviflora Report file
  ###sheet_name - name of a sheet from which the table is to be retrieved
  
  
  #creating cells object
  cells <- xlsx_cells(path)
    
  #filtering out unnecessary data
  summary <- cells %>%
  filter(sheet==sheet_name) %>%
  select(sheet, character, numeric, date, row, col, data_type)
    
  #defining the values of the beginning cell and ending cell of summary table
  first_cell <- 'Summary for Period'
  last_cell <- 'Summary Totals'
   
  #creating a helper table with just the row labels and row and col indexes from the chosen excel sheet
  position_and_row <- summary %>% filter(!is.na(character)) %>% select(character, row, col)
    
  #retrieving the coordinates of the first row label and last row labels of a summary_for_period table
  beginning_row <- stack(setNames(lapply(position_and_row, grep, pattern = first_cell), 'character'))[[1]]
  end_row <- stack(setNames(lapply(position_and_row, grep, pattern = last_cell), 'character'))[[1]]
    
  #selecting only rows that lie between beginning row and end_row, which are part of the summary_for_period table
  position_and_row <- position_and_row %>% slice(beginning_row:end_row) %>% filter(col==1)
    
  #selecting a helper column with rows to lookup the corresponding values  
  rows <- position_and_row %>% select(row)
  
  #creating labels column (removing the first row because it is a name of a table without corresponding value)  
  position <- position_and_row %>% slice(-1) %>% select(character)
  
  #creating column with values by looking up the values corresponding to rows at which labels lie
  trans <- merge(rows, summary[c('numeric', 'row', 'col')], on=row)
  
  #saving each value column to a separate variable
  trans_amount <- trans %>% filter(col==4) %>% slice(-1)
  trans_count <- trans %>% filter(col==5) %>% slice(-1)
  
  #creating a dataframe with the three columns  
  df <- data.frame(position, trans_amount$numeric, trans_count$numeric)
  
  #renaming the columns
  df <- df %>% rename(position=character, trans_amount=trans_amount.numeric, trans_count=trans_count.numeric)
    
  return(df)
}

parse_table <- function(xlsx_path, sheet_name){
  #This function takes Parviflora Financial Report (part with each shops' report) and bundles it together in one table
  #inputs:
  #path - path of a xlsx file
  
  #importing xlsx file and creating a cells object out of it
  cells <- xlsx_cells(xlsx_path)
  
  #setting the value of a first label cell in each individual report to later search for coordinates
  first_cell <- 'Submitting Location:'
  
  #getting rid of unnecessary columns
  summary <- cells %>%
    filter(sheet==sheet_name) %>%
    select(sheet, character, numeric, date, row, col, data_type) %>%
    filter(col %in% c(1,2,3))
  
  #creating a vector with locations of each row containing first_cell value
  beginning_row <- stack(setNames(lapply(summary, grep, pattern = first_cell), 'character'))[[1]]
  
  #removing rows with summary table
  summary <- summary %>% slice(beginning_row[1]:nrow(summary))
  
  #creating helper table with all rows labels and row and col indexes
  labels <- summary %>% filter(!is.na(character)) %>% select(character, row, col)
  
  #creating helper table with all the numeric values from summary table and col and row indexes
  values <- summary %>% select(numeric, row, col)
  
  #creating another helper tables with trans_amount values and trans_count values
  val1 <- values %>% filter(col==2)
  val2 <- values %>% filter(col==3)
  
  #creating a vector with locations of each row containing first_cell value
  #there are six unnedeed labels in each shop's table, those are
  #submitting location:
  #shop id
  #shop code
  #empty
  #transaction amount
  #transaction count
  #code below removes those values
  
  #creating a vector with locations of each row containing first_cell value
  loc <- stack(setNames(lapply(labels, grep, pattern = first_cell), 'character'))[[1]]
  loc_b <- c()
  
  #looping over the locations five times to add the locations of five cells after each occurence of 'Submitting Location:'
  for (i in 1:5) {
    loc_b <- c(loc_b, loc+i)
  }
  
  location <- c(loc, loc_b)
  
  #removing unneeded rows based on the locations calculated above 
  labels_2 <- labels %>% slice(-c(location))
  
  #joining new labels with value tables created above (val1, val2)
  df <- left_join(labels_2, val1, by='row')
  df <- left_join(df, val2, by='row')
  df <- df %>% rename(trans_amount=numeric.x, trans_count=numeric.y)
  
  #extracting ids
  additional_col_names <- labels %>% slice(c(location))
  ids <- additional_col_names %>% filter(col==2 & !character=='Transaction Amount') %>% arrange(row)
  
  #extracting codes
  codes <- additional_col_names %>% filter(col==3 & !character=='Trans Count') %>% arrange(row)
  
  #putting ids in rows where labels == 'Totals' beceause this is the value that is exatcly the same in each table and is there always
  value_to_search_for <- 'Totals'
  intervals <- stack(setNames(lapply(df, grep, pattern = value_to_search_for), 'character'))[[1]]
  
  df$id <- NA
  df$code <- NA
  
  for (i in 1:length(intervals)) {
    df[intervals[i],'id'] <- ids$character[i]
    df[intervals[i], 'code'] <- codes$character[i]
  }
  
  #filling empty rows in id columns with ids 
  df <- df %>% fill(id, .direction='up') %>% filter(!character=='--')
  df <- df %>% fill(code, .direction='up')
  
  #There is a situation where labels duplicate, in those instances value for one label is neagative, because all labels in each table need to be unique,
  #in the code below sufixes are added to labels with negative values
  df_trimmed <- df %>% select(character, trans_amount, trans_count, id, code) %>%
    mutate(trans_amount = replace(trans_amount, is.na(trans_amount), 0)) %>%
    mutate(character_suffix = case_when(trans_amount<0 ~ ' (neg)', TRUE ~ '')) %>%
    mutate(label = paste(character, character_suffix))
  
  #To make the final table there needs to be only one value column, therefore trans_amount and trans_count are going to be merged
  # and additional label column is going to be created
  df_trans_amount <- df_trimmed %>% select(label, trans_amount, id, code)
  df_trans_count <- df_trimmed %>% select(label, trans_count, id, code)
  
  df_trans_amount$position <- 'trans_amount'
  df_trans_count$position <- 'trans_count'
  
  df_trans_amount <- df_trans_amount %>% rename(value=trans_amount)
  df_trans_count <- df_trans_count %>% rename(value=trans_count)
  
  df_final <- rbind(df_trans_amount, df_trans_count)
  
  #pivoting the final table
  a <- spread(df_final, label, value)
  a <- a %>% select(-names(a)[15])
  
  return(a)
}


merge_summaries <- function(xlsx_path){
  #this function creates one summary-for_period combining each sheet of the xlsx file
  #input
  #xlsx_path - path to xlsx file
  
  sheets <- get_sheet_names(xlsx_path)
  
  df_2 <- data.frame(position=c(), trans_amount=c(), trans_count=c())

  for (s in sheets){
    df_1 <- summary_for_period(xlsx_path, s)
    df_1$sheet <- s
    df_2 <- rbind(df_2, df_1)
  }
  return(df_2)
}

df_summary <- merge_summaries(xlsx_path)

df <- parse_table(xlsx_path, sheet_name)
#komentarz próbny