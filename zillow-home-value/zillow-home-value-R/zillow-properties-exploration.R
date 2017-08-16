library(VIM)
library(mice)
library(xlsx)


############################### Data Exploration - Properties ################################################

# Missing values by count in properties dataset
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values, aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()

# Obtain all combination of missingness patterns and their percentages
missing_pattern <- aggr(properties[, !names(properties) %in% missing_values[which(missing_values$missing_pct == 0), 1]], 
                     col = mdc(1:2), numbers = TRUE, labels = names(properties), cex.axis=.7, gap=3,
                     ylab=c("Proportion of missingness","Missingness Pattern"))
missing_comb <- data.frame(miss_pattern$tabcomb)
names(missing_comb)  <- names(miss_pattern$x)
missing_comb$percent <- miss_pattern$percent
write.xlsx2(missing_comb, file = "../output/missing_values_combinations.xlsx", row.names = FALSE)

# Correlation matrix for missingness in data
miss_dummy <- as.data.frame(sapply(properties, function(x){ifelse(is.na(x), 1, 0)}))
corrmat <- cor(miss_dummy[, names(miss_dummy) %in% missing_values[missing_values$missing_pct > 0, 1] ])
write.xlsx2(corrmat, file = "../output/missingness_correlation.xlsx", row.names = TRUE)
ggplot(data = melt(corrmat), aes(x = Var1, y= Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()


# Plot histograms of all variables after filtering NA
properties %>% 
  select_if(is.numeric) %>% 
  select(-one_of(idcol, "censustractandblock", "rawcensustractandblock", "latitude", "longitude")) %>% 
  melt() %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
ggsave(file = "../output/plots/histograms_before_imputation.png", device = "png", width = 16, height = 8, units = "in")



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










