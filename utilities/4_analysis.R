### Analysis 
# This script defines functions used to perform analysis of Parvidlora sales
# create plots etc.
# please use ggplot2 library

get_period_header <- function(df){
  '
  This function creates a string that can be attached to any title and informs about the period which was analyzed.
  returns string e.g: "from January to March 2020"
  '
  first_month <- month.name[select(df, month) %>% min()]
  last_month <- month.name[select(df, month) %>% max()]
  year <- df %>% select(year) %>% max() %>% as.character()
  period_header <- paste("from", first_month, "to", last_month, year)
  
  return(period_header)
}


save_plot <- function(name) {
  '
  Function that saves the LAST GENERATED plot to output/plots directory
  use this function to safely save the plot (in case the path with "/" fails)
  '
  #TODO Is there an option that would make saved plots scale automatically?
  tryCatch(
    expr = {
      ggsave(paste("output", "plots", name, sep = '/'))
    }, # in case the above doesn't work - use recommended "here" library
    error = function(e){
      saveRDS(df_complete, here::here("output", "plots", name))
      message("File saved using here package, couldn't resolve path with '/'")
    }
  )
  
}

horizontal_bar_stores <- function(df, period) {
  '
  This function creates the horizontal bar chart of Total Revenue by store sorted best to worse.
  Same plot for count is rather not necessary - counts are interesting on per flower basis
  '
  # create plot regarding total revenue
  # TODO make it more pretty (delete "Parviflora" maybe?) and add some space on the right side 
  ggplot(df, aes(x = reorder(store_name, rev_total) , y = rev_total)) + 
    geom_bar(stat="identity") + coord_flip() + 
    xlab("Store Name") + ylab("Total Revenue") + ggtitle(paste("Total Revenue of Parviflora stores", period))
  
  save_plot("tot_rev_stores.png")
  
}

gowno_plot <- function(df, period){
  'Create some plots... and save them in the end'
  
  save_plot("blabla.png")
}

