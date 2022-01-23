'
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
'
# ^ wersja do tekstowego edytora z final assigmentem
  
Sys.setlocale("LC_CTYPE", "Polish")
library(tidyverse)
library(readxl)
library(ggplot2)


### SUMMARY OF SALES ####

source(list.files(pattern = "*process_sales_summaries*", recursive = TRUE))
# TODO 

# create a list of all Summary of Sales ... .csv files
files_list <- list.files(pattern="^Summary.*csv$", recursive = TRUE)
df_sales <- union_sales_data(files_list)


### DAFFODILS ####
# notice that only functions appear when sourcing this script - no variables are defined there
source(list.files(pattern = "*process_Daffodils*.r$", recursive = TRUE))

# TODO - do it in utilities/2_..Daffodils.r file


# use recursive = TRUE to get matching files in subdirectories
paths <- list.files(pattern = "^Daffodils.*xls$", recursive = TRUE)

# For now we only have one year - thus one file [1] but what in the future?
# NOT NECESSARY in this project - Create a loop to iterate through these files - [1] indicates this possibility as paths would be a vector if there are many files

df_summary_daffodils <- merge_summaries(paths[1]) 

df_daffodils <- combine_tables(paths[1], totals_only = TRUE)


# Join with stores
### Read in STORES ####
source(list.files(pattern = "*process_Stores*.R$", recursive = TRUE))
# Find Stores data
stores_file <- list.files(pattern="^Stores.*xlsx$", recursive = TRUE)
# read the Stores data.frame - it is now a permutation of all stores with all possible months
stores <- read_stores(stores_file, df_sales, df_daffodils)

# Left join to Stores - all stores will be present at this stage! 
# Even those with all NULLs are present - meaning no sales of other flowers in these stores
# (i.e. later join Daffodil data so we NEED them)
df_stores_sales <- stores %>% left_join(select(df_sales, -"store_name"), 
                                        by = c("store_id" = "store_id",
                                               "month" = "month_id", "year" = "year_id"))

### JOIN Daffodils to Sales Summaries ###
# on store_id, month and year
df_complete <- df_stores_sales %>% full_join(select(df_daffodils, -id), 
                                   by = c("store_id" = "store_id", 
                                          "month" = "month", "year" = "year")) %>%
                                   arrange('store_id', "month")

# Add totals from Daffodils file to their place and drop these columns
df_complete <- df_complete %>% mutate(rev_Daffodil = trans_amount,
                                      count_Daffodil = trans_count) %>%
                               select(-trans_amount, -trans_count) %>%
  # flag stores with missing name as "unknown" 
                               mutate(store_name = ifelse(is.na(store_name), "unknown", store_name)) %>%
  # However now the rev_total is not a really a total... should be updated
                               mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # first replace NAs (NA + 1 = NA)
                               mutate(rev_total = rev_total + rev_Daffodil)
                

# Save checkpoint file to /processed folder after data from both systems was integrated
# this file may as well be used in .Rmd file
tryCatch( # Just an experiment with tryCatch in R ... ugly syntax
    expr = {
      saveRDS(df_complete, file = "data/processed/integrated_data.rds")
    }, # in case the above doesn't work - use recommended "here" library
    error = function(e){
      saveRDS(df_complete, here::here("data", "processed", "integrated_data.rds"))
      message("File saved using here package, couldn't resolve path with '/'")
    }
  )

# save .csv file as well in case anyone not familiar with R would like to make own analysis e.g. in Excel
write.csv2(df_complete, 'output/integrated_data.csv')


### ANALYSIS ###
# output plots / files whatever to /output directory
source(list.files(pattern = "4_analysis*.R$", recursive = TRUE))

# I think we should drop NAs from store_name and report them separately (KUBA) - flagged as "unkknown"
df_analysis <- df_complete %>% filter(store_name != "unknown")

p <- get_period_header(df_analysis)
horizontal_bar_stores(df_analysis, period = p)

# TODO Stores that didn't provide us with data in Total Sales Summary
# e.g. store_id 345 doesn't exist in Stores data we need to flag those
# dla Swiebodzina brak danych wszędzie
# Kuba - przeniosłem tą część tutaj bo bardziej pasuje to do analizy
which(is.na(df_stores_sales$store_number)) # tych NA będzie teraz więcej bo są tez te z Daffodils
number <- df_stores_sales$store_number

#for(i in 1:length(df_stores_sales$store_number)) {
#  if(is.na(data$x_num[i])) {
#    print("Damn, it's NA")
#  }


