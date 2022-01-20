---
  title: "Parviflora Project"
subtitle: "Group project for Introduction to R classes"
author: "Jakub Zapasnik (38401), Maciej Golik (46827), Paweł Jędrzejczak (46471), Daniel Lilla (38963), Michał Kloska (46341)"
output: 
  html_document:
  number_sections: true
editor_options: 
  chunk_output_type: console
---
# ^ wersja do tekstowego edytora z final assigmentem
  
Sys.setlocale("LC_CTYPE", "Polish")
library(tidyverse)
library(readxl)


### SUMMARY OF SALES ####

source(list.files(pattern = "*process_sales_summaries*", recursive = TRUE))
# TODO do sprawdzenia i zdefiniowania typy danych:
# Pozdro - wczytujemy sobie dane ale one mają zły typ 
# class(df_sales$count_total) to samo rev_total

# create a list of all Summary of Sales ... .csv files
files_list <- list.files(pattern="^Summary.*csv$", recursive = TRUE)
df_sales <- union_sales_data(files_list)


# Join with stores
### Read in STORES ####
# LUDZIE NA WINDOWSIE - sprobujcie wczytac plik po zakomentowanej linii - pewnie nie zadziała
#stores <- readxl::read_excel("data/Stores.xlsx")

# list.files zwraca sciezke która będzie ok na większosci OS
stores_file <- list.files(pattern="^Stores.*xlsx$", recursive = TRUE)
stores <- readxl::read_excel(stores_file)

# Left join to Stores - all stores will be present at this stage! 
# Even those with all NULLs are present - meaning no sales of other flowers in these stores
# (i.e. later join Daffodil data so we NEED them)
df_stores_sales <- stores %>% left_join(select(df_sales, -"store_name"), 
                                        by = c("Store ID" = "store_id"))



### DAFFODILS ####
# notice that only functions appear when sourcing this script - no variables are defined there
source(list.files(pattern = "*process_Daffodils*.r$", recursive = TRUE))

# TODO - do it in utilities/2_..Daffodils.r file
#   - Change dtype of "code" to numeric (as in Summary data.frames)
#   - Change column month to numeric type (ready variable - just assign to column)
#   - add column "year" 
#   - add column - "sheet" for easier identification only - it's current month column in "Feb20" format


# one solution - use recursive = TRUE to get matching files in subdirectories
paths <- list.files(pattern = "^Daffodils.*xls$", recursive = TRUE)

# TODO For now we only have one year - thus one file [1] but what in the future?
# Create a loop to iterate through these files - NOT NECESSARY in this project

df_summary <- merge_summaries(paths[1]) 

df_final <- combine_tables(paths[1], totals_only = TRUE)

### JOIN Daffodils to Sales Summaries ###
# TODO First fix data types
# Later join on Store ID, month_id and year
#class(df_stores_sales$`Store ID`)


### ANALYSIS ###
# Separate .r script in /utilities folder to make plots etc.
# output plots / files whatever to /output directory
# Stores that didn't provide us with data in Total Sales Summary

# Kuba - przeniosłem tą część tutaj bo bardziej pasuje to do analizy
which(is.na(df_stores_sales$store_number))
number <- df_stores_sales$store_number

#for(i in 1:length(df_stores_sales$store_number)) {
#  if(is.na(data$x_num[i])) {
#    print("Damn, it's NA")
#  }


