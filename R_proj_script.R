sales_feb_2020 <- read.csv("Summary of Sales February 2020.csv")
sales_jan_2020 <- read.csv("Summary of Sales January 2020.csv")
sales_mar_2020 <- read.csv("Summary of Sales March 2020.csv")
stores <- readxl::read_excel("Stores.xlsx")

# Michau to jest gosc

#jebac psy
x == 1

<<<<<<< HEAD
#jebac pis
#stonoga mial racje
=======
#jebać psu 
y == 2

#komentarz Macieja
>>>>>>> f416cf5394b2c2b9bdda7e7302084a283be6ea16

#R jebie dupsko

Sys.setlocale("LC_CTYPE", "Polish")
library(haven)
library(tidyverse)

#Still can't figure out how to change letters t o polish ones and replace the "?" signs. 

as.character(stores$"Store Name") 
as.integer(stores$"Store ID")

names(stores) <- c("Store ID", "STORE.NAME")

sales_jan_2020_test= sales_jan_2020 %>% left_join(stores,by="STORE.NAME")