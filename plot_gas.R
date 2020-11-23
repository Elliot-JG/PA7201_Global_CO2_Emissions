library(tidyverse)

plot_gas <- function(file, gas, gas_unit, region) {

#Read csv
plot_init <- read_csv(file)

#Create fixed table to plot (date column class is date)
table <- plot_init %>%
  
  #date column renamed, gas column renamed to gas input
  rename(date = 'system:time_start', gas = undefined) %>%
  
  # Remove NA's
  filter(!is.na(gas)) %>%
  
  # Convert date column to date format 
  mutate(
    date_real = as.Date(date, "%B %d, %Y")
  ) %>%
  
  # Compute averages for each day, by grouping
  group_by(date_real) %>%
  summarise(
    Avg_gas = mean(gas)
  )

#Plot 

plot <- table %>%
  ggplot(aes(
    
    # Define axis 
    x = date_real, 
    y = Avg_gas)) +
  
  # Set colour and size of the line 
  geom_line(
    colour = '#3366cc', 
    size = 0.75) +
  
  # Scale x axis by month 
  scale_x_date(
    date_breaks = "1 month",
    
    # Display months by first three letters on axis 
    date_labels = "%b") +
  
  # Set x and y axis labels 
  labs(
    x = "Month in 2020", 
    y = expression(Daily~average~CO~column~density~(mol/m^2)))

  

  return(plot)

}


