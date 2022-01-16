#Loading all files

library(tidyverse)
sales_jan_2020 <- read.csv("Summary of Sales January 2020.csv")
sales_feb_2020 <- read.csv("Summary of Sales February 2020.csv")
sales_mar_2020 <- read.csv("Summary of Sales March 2020.csv")
stores <- readxl::read_excel("Stores.xlsx")
Sys.setenv("LANGUAGE"="PL")
library(dplyr)

#JANUARY 

rename(sales_jan_2020, "Stores Name" = STORE.NAME)
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

# adding additional columns which contains the shop numbers
sales_jan_2020 <- sales_jan_2020 %>% add_column(`Store Number` = stores_numbers)
sales_jan_2020 <- sales_jan_2020[,-1]

# relocating added column to the front
sales_jan_2020 <- sales_jan_2020 %>% relocate(`Store Number.1`, .before = 'Number of orders (Azalea)')
sales_jan_2020 <- rename(sales_jan_2020, "Store ID" = 'Store Number.1')

# merging tables - ale to jest shit bo te co ie ma numerów to nie pokazuje
sales_jan_2020 <- merge(sales_jan_2020,stores,by='Store ID')
sales_jan_2020 <- sales_jan_2020 %>% relocate(`Store Name`, .before = 'Store ID')


# FEBRUARY


rename(sales_feb_2020, "Stores Name" = STORE.NAME)
# subtracting those columns which are unnecessary
sales_feb_2020 <- sales_feb_2020[,-c(1,9:10)]

# renaming columns - aesthetics reasons 
sales_feb_2020 <-rename(sales_feb_2020, "Store Number" = STORE.., 
                        "Number of orders (Azalea)" = COUNT,
                        "Number of orders (Begonia)" = COUNT.1,
                        "Number of orders (Carnation)" = COUNT.2,
                        "Total number of the orders" = COUNT.4)

# subtracting all of the digits from store number, except of last 3 
stores_numbers <- str_sub(sales_feb_2020$`Store Number`,-3)

# adding additional columns which contains the shop numbers
sales_feb_2020 <- sales_feb_2020 %>% add_column(`Store Number` = stores_numbers)
sales_feb_2020 <- sales_feb_2020[,-1]

# relocating added column to the front
sales_feb_2020 <- sales_feb_2020 %>% relocate(`Store Number.1`, .before = 'Number of orders (Azalea)')
sales_feb_2020 <- rename(sales_feb_2020, "Store ID" = 'Store Number.1')

# merging tables - ale to jest shit bo te co ie ma numerów to nie pokazuje
sales_feb_2020 <- merge(sales_feb_2020,stores,by='Store ID')
sales_feb_2020 <- sales_feb_2020 %>% relocate(`Store Name`, .before = 'Store ID')


#MARCH


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

# adding additional columns which contains the shop numbers
sales_mar_2020 <- sales_mar_2020 %>% add_column(`Store Number` = stores_numbers)
sales_mar_2020 <- sales_mar_2020[,-1]

# relocating added column to the front
sales_mar_2020 <- sales_mar_2020 %>% relocate(`Store Number.1`, .before = 'Number of orders (Azalea)')
sales_mar_2020 <- rename(sales_mar_2020, "Store ID" = 'Store Number.1')

# merging tables - ale to jest shit bo te co ie ma numerów to nie pokazuje
sales_mar_2020 <- merge(sales_mar_2020,stores,by='Store ID')
sales_mar_2020 <- sales_mar_2020 %>% relocate(`Store Name`, .before = 'Store ID')

