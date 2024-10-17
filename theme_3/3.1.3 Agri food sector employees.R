#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_3_1_4 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_3/input_data/3_1_9b_agri_food_sector_employees_self_employed_farmers_time.csv")

FSR_3_1_4 <- FSR_3_1_4 %>%
  gather(variable,value, `Agriculture (including fishing)`,`Food and drink manufacturing`,`Food and drink wholesaling`,`Food and drink retailing`,`Food and drink non-residential catering`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

FSR_3_1_4_plot <- ggplot(FSR_3_1_4, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  labs(x = NULL,
       y = "Thousand people") +
  scale_colour_manual(values = af_colours()) + 
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") 

FSR_3_1_4_plot

save_graphic(FSR_3_1_4_plot, '3.1.3a', ' Agri food sector employees self employed farmers') + 
  save_csv(FSR_3_1_4, '3.1.3a', ' Agri food sector employees self employed farmers')
