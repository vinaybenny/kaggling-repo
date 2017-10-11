############################### Data Exploration - Transactions ################################################

# Plot count of sales transactions for each month
transactions %>% 
  mutate(year_month = paste0(as.character(year(date)), "-", 
                             ifelse(nchar(as.character(month(date))) > 1, 
                                    as.character(month(date)), paste0("0",as.character(month(date))) ))) %>%
  group_by(year_month) %>% 
  count() %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-10-01"))), size = 2)

# Properties with multiple sales
transactions %>% group_by(id_parcel) %>% summarise(ct = n() ) %>% filter(ct > 1) %>% arrange( desc(ct) )

# Create plots of each numeric variable against mean absolute log error
for ( i in 1:length(numcols) ) {
  print(i)
  
  # Create variable v/s logerror datasets for plotting
  plotdata <- transactions %>%
    group_by_( numcols[i] ) %>%
    summarise(mean_abs_logerror = mean(abs(logerror)), mean_logerror = mean(logerror), ct = n())
  names(plotdata)[1] <- numcols[i]
  # Plots for absolute logerror
  ggplot(data = plotdata, aes(x = get(numcols[i]), y = mean_abs_logerror)) +
    geom_smooth(color = "grey40") +
    geom_point(color = "red") +
    xlab(numcols[i]) +
    coord_cartesian(ylim = c(0, 1.00)) +
    theme_bw()
  ggsave(filename = paste("../output/plots/var_abs_", numcols[i], ".png"), device = "png")
  #Plots for actual logerror
  ggplot(data = plotdata, aes(x = get(numcols[i]), y = mean_logerror)) +
    geom_smooth(color = "grey40") +
    geom_point(color = "blue") +
    xlab(numcols[i]) +
    coord_cartesian(ylim = c(-1.00, 1.00)) +
    theme_bw()
  ggsave(filename = paste("../output/plots/var_", numcols[i], ".png"), device = "png")
}



# Try a glm model on the dataset
train_x$logerror <- train_y
res.lm  <-lm(formula = logerror ~ ., data = train_x)
cooksd <- cooks.distance(res.lm)

# Remove extreme outliers from train_x
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 10*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

x <- data.frame(col = as.numeric(names(cooksd)), value = cooksd)
y <-x %>% filter(!is.na(value)) %>% filter(value > quantile(value, prob = c(0.99))) %>% select(col)
train_x <- train_x[-y$col,]
train_y <- train_y[-y$col]

#=================================================================================================================================

col<- rainbow(255,end=5/6) 

colid <- function( x, range=NULL, depth=255 ) 
{ 
  if ( is.null( range ) ) 
    y <- as.integer(x-min(x))/(max(x)-min(x))*depth+1 
  else 
  { 
    y <- as.integer(x-range[1])/(range[2]-range[1])*depth+1 
    y[ which( y < range[1] ) ] <- 1 
    y[ which( y > range[2] ) ] <- depth 
  } 
  y 
} 

plot( transactions$x_coord,transactions$y_coord,col=col[ colid(as.numeric(cut_number(transactions$logerror, 10))) ] ) 

transactions %>% mutate(temp = as.numeric(cut_number(logerror, 5))) %>% ggplot(aes(x_coord, y_coord, color = temp)) + 
  geom_point()



