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
library(haven)
library(tidyverse)
library("xlsx")
getwd()
#^to check your repository



###STORES####

stores <- readxl::read_excel("Stores.xlsx")
names(stores) <- c("Store ID", "STORE.NAME")
#ignore.case = TRUE

#patterns <- c("*110", "*170", "*390", "*400", "*410", "*420", "*430", "*440", "*510",
#              "*150", "*210", "*240", "*350", "*200", "*260", "*330", "*540", "*550", 
#              "*570", "*580", "*590", "*270", "*320", "*340", "*500", "*360", "*530")
#result <- filter(stores, grepl(paste(patterns, collapse="|"), Letter))
#w trakcie robotaju

###SUMMARY OF SALES####

temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

# ^ This will automaticly load any csv file 

Sales <- list.files(path = getwd(), pattern = "*.csv")

###DAFFODILES####

Daffodils <- list.files(pattern="xls$")

Daffodils2020 <- map_df(f, read_excel)


