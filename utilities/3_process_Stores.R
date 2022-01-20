### Stores data ### 

read_stores <- function(path, df_sales, df_daffodils) {
  stores <- readxl::read_excel(path)
  # fix data types
  stores <- stores %>% dplyr::rename(store_id = 'Store ID', store_name = 'Store Name') %>%
          mutate(across(store_id, as.integer))


# TODO in the future here should be the combination of month + year
# create a list of all months that appear in both data sources
all_mths <- union(unique(df_sales$month_id), unique(df_daffodils$month)) %>% sort()
yr <- unique(df_sales$year_id)

# TODO maybe there is a way to do element by element multiplication and achieve same result easier...
# for now to get all combinations we add columns and pivot
# create a new column for each month
for (m in all_mths) {
  col_nm <- paste("month",m, sep='')
  stores <- stores %>% add_column("{col_nm}" := m)
}

# pivot the table and delete the extra column - now we have a combination of all stores and possible months
stores <- stores %>% pivot_longer(c(3:(length(all_mths)+2)) ,names_to = "colnm", values_to = "month") %>% 
                     select(-colnm) %>% mutate(year = yr)

return(stores)
}
