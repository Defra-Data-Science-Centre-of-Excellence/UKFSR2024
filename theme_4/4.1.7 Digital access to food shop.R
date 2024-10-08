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

contents <- get_bucket_df("s3-ranch-054")



FSR_4_1_7 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Digital access to food shop dataset.csv",
                                  col_names = TRUE, 
                                  skip = 5)

colnames(FSR_4_1_7) <- c("Year", "All retailing", "Food stores")
FSR_4_1_7 <- FSR_4_1_7[, c(1:3)]

FSR_4_1_7 <- FSR_4_1_7 %>%
  gather(variable,value, `All retailing`,`Food stores`) %>%
  mutate(Year = as.Date(as.yearmon(Year, "%Y %b")))


FSR_4_1_7_plot <- ggplot(FSR_4_1_7, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),date_labels = "%Y") +  # Corrected `date_labels`
  labs(x = NULL,
       y = "Percent (%)") +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_1_7_plot


save_graphic(FSR_4_1_7_plot, '4.1.7', ' Internet Sales as a propotion of all retailing ') + 
  save_csv(FSR_4_1_7, '4.1.7', 'Internet Sales as a propotion of all retailing')

---------------------------------------------------------------------------------------------------------------------------------------------------------

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
            size = 7, color = "white") +  
  coord_flip() +
  scale_fill_manual(values = rev(af_colours()[1:5])) +
  labs(y = "% of sales by type of food shop",
       x = NULL,
       fill = "Retail Channel") +
  theme_ukfsr()

FSR_4_1_7a_plot

save_graphic(FSR_4_1_7a_plot, '4.1.7a','Usage proportion of retail channels in %') + 
  save_csv(FSR_4_1_7a, '4.1.7a','Usage proportion of retail channels in %')