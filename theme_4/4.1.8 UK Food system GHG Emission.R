library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_4_1_8a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/GHG emissions chart.csv")

FSR_4_1_8a <- FSR_4_1_8a %>%
  gather(variable,value, `UK Agriculture`,`Imports`,`Other inputs`,`Supply chain and consumer`,`Disposal`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

FSR_4_1_8_plot <-  ggplot(FSR_4_1_8a, aes(x = Year, y = value, fill = variable)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_colour_manual(values = af_colours("categorical")) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  labs(
    x = NULL,
    y = "GHG Emissions (MtCO2e)",
    fill = "Category"
  ) +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_1_8_plot

save_graphic(FSR_4_1_8_plot, '4.1.8a', 'UK Food system GHG Emission') + 
  save_csv(FSR_4_1_8_plot, '4.1.8a', ' UK Food system GHG Emission')