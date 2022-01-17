sales_feb_2020 <- read.csv("Summary of Sales February 2020.csv")
sales_jan_2020 <- read.csv("Summary of Sales January 2020.csv")
sales_mar_2020 <- read.csv("Summary of Sales March 2020.csv")
stores <- readxl::read_excel("Stores.xlsx")
daffodiles <- readxl::read_excel("Daffodils2020.xls")
Sys.setlocale("LC_CTYPE", "Polish")
library(haven)
library(tidyverse)

#Still can't figure out how to change letters t o polish ones and replace the "?" signs. 

as.character(stores$"Store Name") 
as.integer(stores$"Store ID")

names(stores) <- c("Store ID", "STORE.NAME")

sales_jan_2020_test= sales_jan_2020 %>% left_join(stores,by="STORE.NAME")


###STORES####

get_file_paths_stores <- function(pattern = "Stores.xlsx"){
  stores_file_paths <- c()
  for(f in Sys.glob(pattern)){
    stores_file_paths <- c(stores_file_paths,
                              paste(normalizePath(dirname(f)), fsep = .Platform$file.sep, f, sep = "")) 
  }
  return(stores_file_paths)
}


###SUMMARY OF SALES####

get_file_paths_sales <- function(pattern = "Summary of Sales*.csv"){
  summary_file_paths <- c()
  for(f in Sys.glob(pattern)){
    summary_file_paths <- c(stores_file_paths,
                           paste(normalizePath(dirname(f)), fsep = .Platform$file.sep, f, sep = "")) 
  }
  return(summary_file_paths)
}

convert_to_csv <- function(csv_path) {
  temp_xls <- rio::import_list(xls_path, setclass = "tbl")
  yr <- stringr::str_match(csv_path, "Summary of Sales\\s*(.*?)\\s*.csv")[,2]
  conv_file = paste("Summary of Sales", mm,  yr, ".csv", sep='')
  rio::export(temp_xls, conv_file)
  return(conv_file)
}

get_sheet_names <- function(path){
  #This function returns names of all the sheets in the excel files
  #inputs:
  #path - path of a xlsx file
  
  cells <- CSV_cells(path)
  
  return(unique(cells$sheet))
}

### ??????????????????????????????????????????? WAT
paths <- get_file_paths_sales()

###DAFFODILES####

get_file_paths_daffodiles <- function(pattern = "Daffodils*.xls"){
  daffodils_file_paths <- c()
  for(f in Sys.glob(pattern)){
    daffodils_file_paths <- c(daffodils_file_paths,
                              paste(normalizePath(dirname(f)), fsep = .Platform$file.sep, f, sep = "")) 
  }
  return(daffodils_file_paths)
}


names(sales_feb_2020)


