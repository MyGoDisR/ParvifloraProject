### Stores data ### 
# This script defines functions needed to pre-process "Stores.xlsx" file

read_stores <- function(path, df_sales, df_daffodils) {
  '
  Function which reads the Stores.xlsx file specified in path argument 
  and cross-joins it with all month-year combinations found in Summary and Daffodils data. 
  This assures that the joins happening later will not be joined with NA data (resulting in extra rows), 
  but instead they will be present at the level of current store_id, month and year.
  Year column is not currently necessary, but it would allow for desirable behaviour even if data from multiple years was present.
  Inputs:
    -> path - the path to .xlsx file (relative from root directory of the project)
    -> df_sales - data.frame of sales data obtained from previous processing (actually we may change to month and years cols only)
    -> df_daffodils - data.frame of Daffodils sales data obtianed from previous processing (as above)
  Output:
    <- stores - data.frame with all possible combinations of store_id-month-year
  '
  stores <- readxl::read_excel(path)
  # fix data types
  stores <- stores %>% dplyr::rename(store_id = 'Store ID', store_name = 'Store Name') %>%
          mutate(across(store_id, as.integer))

  # create a tibbles of all month-year that appear for each data source - the column "m_y" in format "1_2020"
  s <- df_sales %>% select(month_id, year_id) %>% unique() %>% unite(m_y, c(1,2), sep="_")
  d <- df_daffodils %>% select(month, year) %>% unique() %>% unite(m_y, c(1,2), sep="_")
  # perform cross-join operation on a union of above variables and later split "m_y" column into separate numeric columns
  stores <- stores %>% tidyr::crossing(union(s,d)) %>% separate(m_y, sep="_", 
                                                      into = c("month", "year"),
                                                      convert = TRUE) # this will convert month and year to numeric type
                                        
  # Just to check if it works for multiple years
  # z <- rbind(union(s,d), list(c("4_2020", "1_2021", "2_2021")))
  
  return(stores)
}
