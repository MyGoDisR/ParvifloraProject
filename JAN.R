library(tidyverse)
sales_jan_2020 <- read.csv("Summary of Sales January 2020.csv")
stores <- readxl::read_excel("Stores.xlsx")
Sys.setenv("LANGUAGE"="PL")
library(dplyr)
rename(sales_jan_2020, "Stores Name" = STORE.NAME)
# temporary solution only used in January
rownames(sales_jan_2020) <- c(
  "PARVIFLORA ŁOMŻA",
  "PARVIFLORA SwIEBODZIN",
  "PARVIFLORA BOGATYNIA",
  "PARVIFLORA OSTROŁĘKA GROSS",
  "PARVIFLORA LUBLIN GROSS",
  "PARVIFLORA LUBLIN RETAIL",
  "PARVIFLORA KATOWICE",
  "PARVIFLORA ŁÓDŻ",
  "PARVIFLORA GDAŃSK",
  "PARVIFLORA SZCZECIN",
  "PARVIFLORA POZNAŃ",
  "PARVIFLORA KRAKÓW",
  "PARVIFLORA SUWAŁKI",
  "PARVIFLORA ŁOMŻA GROSS",
  "PARVIFLORA TORUŃ",
  "PARVIFLORA OSTROŁĘKA",
  "PARVIFLORA WĄCHOCK",
  "PARVIFLORA BIAŁYSTOK",
  "PARVIFLORA KIELCE",
  "PARVIFLORA KIELCE GROSS",
  "PARVIFLORA BYDGOSZCZ",
  "PARVIFLORA GORZÓW WLKP.",
  "PARVIFLORA WARSZAWA",
  "PARVIFLORA WARSZAWA OCHOTA",
  "PARVIFLORA KUTNO",
  "PARVIFLORA WARSZAWA GROSS",
  "PARVIFLORA TORUŃ GROSS",
  "PARVIFLORA BOGATYNIA GROSS",
  "PARVIFLORA WĄCHOCK GROSS",
  "PARVIFLORA WROCŁAW",
  "PARVIFLORA BYDGOSZCZ GROSS",
  "PARVIFLORA KUTNO GROSS",
  "PARVIFLORA CHEŁM OTHER",
  "PARVIFLORA CHEŁM GROSS",
  "PARVIFLORA CHEŁM RETAIL",
  "PARVIFLORA")
# subtracting those columns which are unnecessary
sales_jan_2020 <- sales_jan_2020[,-c(1,9:10)]
# renaming columns - aesthetics reasons 
sales_jan_2020 <-rename(sales_jan_2020, "Store Number" = STORE.., 
                        "Number of orders (Azalea)" = COUNT,
                        "Number of orders (Begonia)" = COUNT.1,
                        "Number of orders (Carnation)" = COUNT.2,
                        "Total number of the orders" = COUNT.4)
# subtracting all of the digits from store number, except of last 3 
stores_numbers <- str_sub(sales_jan_2020$`Store Number`,-3)
library(dplyr)
# adding additional columns which contains the shop numbers
sales_jan_2020 <- sales_jan_2020 %>% add_column(`Store Number` = stores_numbers)
sales_jan_2020 <- sales_jan_2020[,-1]
# relocating added column to the front
sales_jan_2020 <- sales_jan_2020 %>% relocate(`Store Number.1`, .before = 'Number of orders (Azalea)')
view(sales_jan_2020)
