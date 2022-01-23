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

diverging_bar_stores <- function(df) {
  '
  Function creates Diverging bar chart as shown here: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
  Input:
    -> Parviflora dataframe in its final form
  Output:
    <- png. file with the chart
  '
  
  #preparing data
  df_plot_prep <- df %>%
    dplyr::group_by(store_name) %>% 
    dplyr::summarise(revenue=sum(rev_total)) %>% #grouping by store_name and summarizing with sum of total revenues for the stores
    mutate(revenue_norm = round((revenue - mean(revenue))/sd(revenue), 2)) %>% #calculating normalized revenue equation and rounding to two decimal places
    mutate(flag = case_when(revenue_norm < 0 ~ "below", revenue_norm > 0 ~ "above")) %>% #creating flag column with "above" for stores with higher revenue than average and "below" for lower
    dplyr::arrange(revenue_norm) %>% #sorting by revenue_norm
    dplyr::mutate(store_name = factor(store_name, level=store_name)) #converting store_name to factor, because the plot doesn't sort bars properly without it
  
  
  # Diverging Barcharts
  ggplot(df_plot_prep, aes(x=store_name, y=revenue_norm, label=revenue_norm)) + 
    geom_bar(stat='identity', aes(fill=flag), width=.5)  +
    scale_fill_manual(name="Revenue", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#53AD70", "below"="#FA9391")) + 
    labs(subtitle="Normalized Revenue per Store (0 = average revenue)", 
         title= "Revenues") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  +
    coord_flip()
  
  save_plot("Normalized_Revenue_per_Store.png")
}


bar_order_flower <- function(df) {
  '
  Function first creates data with mean order value for each flower (revenues / count), then plots it
  NIE MAM ZBYTNIO POMYSŁU NA TEN WYKRES, BO NA TĄ CHWILĘ WYGLĄDA ŚREDNIO XD
  Input:
    -> Parviflora dataframe in its final form
  Output:
    <- png. file with the chart
  '
  
  df_order <- df_analysis %>%
    dplyr::mutate(Azalea = rev_Azalea/count_Azalea,
                  Begonia = rev_Begonia/count_Begonia,
                  Carnation = rev_Carnation/count_Carnation,
                  Daffodil = rev_Daffodil/count_Daffodil) %>%
    tidyr::pivot_longer(Azalea:Daffodil, names_to = 'flower', values_to = 'order_value')  %>%
    dplyr::group_by(flower) %>%
    dplyr::summarize(mean_order_value = mean(order_value, na.rm = TRUE))

  ggplot(df_order, aes(x = flower, y = mean_order_value)) + geom_bar(stat = "identity")
  
  save_plot("Mean_Order_per_Flower.png")
  
}


