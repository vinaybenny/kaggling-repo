

############################### Data Exploration - Properties ################################################

# Plot histograms of all variables after filtering NA
properties %>% 
  select_if(is.numeric) %>% 
  select(-one_of(idcol, "censustractandblock", "rawcensustractandblock", "latitude", "longitude")) %>% 
  melt() %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) + facet_wrap(~variable,scales = "free") + geom_histogram()
ggsave(file = "../output/plots/histograms_before_imputation.png", device = "png", width = 16, height = 8, units = "in")














