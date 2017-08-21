############################### Data Exploration - Transactions ################################################

# Plot count of sales transactions for each month
transactions %>% 
  mutate(year_month = paste0(as.character(year(date)), "-", 
                             ifelse(nchar(as.character(month(date))) > 1, as.character(month(date)), paste0("0",as.character(month(date))) ))) %>%
  group_by(year_month) %>% 
  count() %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-10-01"))), size = 2)

# Properties with multiple sales
transactions %>% group_by(id_parcel) %>% summarise(ct = n() ) %>% filter(ct >1) %>% arrange( desc(ct) )

# Create plots of each numeric variable against mean absolute log error
for ( i in 1:length(data_numcols) ) {
  print(i)
  plotdata <- transactions %>%
    group_by_(get(data_numcols[i])) %>%
    summarise(mean_abs_logerror = mean(abs(logerror)), ct = n())
  names(plotdata)[1] <- data_numcols[i]
  
  ggplot(data = plotdata, aes(x = get(data_numcols[i]), y = mean_abs_logerror)) +
    geom_smooth(color = "grey40") +
    geom_point(color = "red") +
    xlab(data_numcols[i]) +
    coord_cartesian(ylim = c(0, 0.25)) +
    theme_bw()
  ggsave(filename = paste("../output/plots/var_", data_numcols[i], ".png"), device = "png")
}


# Try a glm model on the dataset
res.glm  <- glm(formula = logerror ~ ., data = transactions)