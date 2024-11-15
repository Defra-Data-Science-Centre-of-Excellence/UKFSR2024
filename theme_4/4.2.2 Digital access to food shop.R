# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('scales')
library('zoo')

source(here::here("utils", "load-font.R"))

# % internet sales -------------------------------------------------------------

FSR_4_2_2a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Digital access to food shop dataset.csv",
                                  col_names = TRUE, 
                                  skip = 5)

colnames(FSR_4_2_2a) <- c("Year", "All retailing", "Food stores")
FSR_4_2_2a <- FSR_4_2_2a[, c(1:3)]

FSR_4_2_2a <- FSR_4_2_2a %>%
  gather(variable,value, `All retailing`,`Food stores`) %>%
  mutate(Year = as.Date(as.yearmon(Year, "%Y %b")))


FSR_4_2_2a_plot <- ggplot(FSR_4_2_2a, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(limits = c(0,40) ,labels = function(x) paste0(x, "%"))+
  scale_x_date(breaks = seq(min(FSR_4_2_2a$Year), max(FSR_4_2_2a$Year), by = "2 years"),date_labels = "%Y") + 
  labs(x = NULL,
       y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_2_2a_plot


save_graphic(FSR_4_2_2a_plot, '4.2.2a', ' Internet Sales as a propotion of all retailing') 
save_csv(FSR_4_2_2a, '4.2.2a', 'Internet Sales as a propotion of all retailing')

# Urban rural % spend ----------------------------------------------------------

# Kantar data on proportion of usage of the retail channels in %


FSR_4_1_7a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/Urban_Rural_Demographic_spend.csv")


FSR_4_1_7a <- FSR_4_1_7a %>%
  pivot_longer(cols = c(`Supermarket`, `Convenience`, `Internet`, `Discounter`, `High street`), 
               names_to = "retail_channel", 
               values_to = "proportion")

FSR_4_1_7a$Demography <- factor(FSR_4_1_7a$Demography, 
                                levels = rev(c("City", "Urban", "Suburban", "Semi-rural", "Rural")))


# Plot the bar chart
FSR_4_1_7a_plot <- ggplot(FSR_4_1_7a, aes(x = Demography, y = proportion, fill = retail_channel)) +
  geom_bar(stat = "identity", position = "stack") + 
  geom_text(aes(label = round(proportion, 1)),  # Add text labels
            position = position_stack(vjust = 0.5),  # Position labels in the middle of each bar segment
            size = 7, color = "white", family = "GDS Transport Website") +  
  coord_flip() +
  scale_fill_manual(values = rev(af_colours()[1:5])) +
  labs(y = "% of sales by type of food shop",
       x = NULL,
       fill = "Retail Channel") +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE)

FSR_4_1_7a_plot

save_graphic(FSR_4_1_7a_plot, '4.2.2b','Usage proportion of retail channels in %')
save_csv(FSR_4_1_7a, '4.2.2b','Usage proportion of retail channels in %')
