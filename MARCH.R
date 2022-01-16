library(tidyverse)
sales_mar_2020 <- read.csv("Summary of Sales March 2020.csv")
stores <- readxl::read_excel("Stores.xlsx")
Sys.setenv("LANGUAGE"="PL")
library(dplyr)
rename(sales_mar_2020, "Stores Name" = STORE.NAME)
# subtracting those columns which are unnecessary
sales_mar_2020 <- sales_mar_2020[,-c(1,9:10)]
# renaming columns - aesthetics reasons 
sales_mar_2020 <-rename(sales_mar_2020, "Store Number" = STORE.., 
                        "Number of orders (Azalea)" = COUNT,
                        "Number of orders (Begonia)" = COUNT.1,
                        "Number of orders (Carnation)" = COUNT.2,
                        "Total number of the orders" = COUNT.4)
# subtracting all of the digits from store number, except of last 3 
stores_numbers <- str_sub(sales_mar_2020$`Store Number`,-3)
library(dplyr)
# adding additional columns which contains the shop numbers
sales_mar_2020 <- sales_mar_2020 %>% add_column(`Store Number` = stores_numbers)
sales_mar_2020 <- sales_mar_2020[,-1]
# relocating added column to the front
sales_mar_2020 <- sales_mar_2020 %>% relocate(`Store Number.1`, .before = 'Number of orders (Azalea)')
view(sales_mar_2020)


