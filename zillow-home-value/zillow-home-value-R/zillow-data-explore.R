
# Count of sales transactions for each month
transactions %>%
  group_by(year_month) %>% count() %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-10-01"))), size = 2)


# Create plots of each numeric variable against mean absolute log error
data_numcols <- names(properties[, sapply(properties, is.numeric) & !(names(properties) == "id_parcel")])

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

# Missing values by count
missing_values <- transactions %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values, aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
geom_bar(stat="identity", fill ="red") +
coord_flip()

good_features <- filter(missing_values, missing_pct<=0.75)
