# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))


# GHG emissions ----------------------------------------------------------------

FSR_4_1_10 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/GHG emissions chart.csv")

FSR_4_1_10 <- FSR_4_1_10 %>%
  gather(variable,value, `UK Agriculture`,`Imports`,`Other inputs`,`Supply chain and consumer`,`Disposal`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 


FSR_4_1_10_plot <-  ggplot(FSR_4_1_10, aes(x = Year, y = value, fill = variable)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_fill_manual(values = af_colours("categorical")) + 
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "1 year"),date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = "GHG Emissions (MtCO2e)",
    fill = "Category"
  ) +
  theme_ukfsr(base_family = "GDS Transport Website")  +
  guides(fill = guide_legend(nrow = 2))

FSR_4_1_10_plot

save_graphic(FSR_4_1_10_plot, '4.3.3a', 'UK Food system GHG Emission') 
save_csv(FSR_4_1_10, '4.3.3a', ' UK Food system GHG Emission')

#  Area of deforestation associated with UK consumption ------------------------
  
FSR_4_1_10a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_4/input_data/Deforestation.csv")

FSR_4_1_10a_plot <-  ggplot(FSR_4_1_10a, aes(x = Year, y = `Deforestation (ha)`)) +
  geom_line(color = af_colours()[1]) +
  scale_x_continuous(breaks = seq(min(FSR_4_1_10a$Year), max(FSR_4_1_10a$Year), by = 2)) +
  scale_y_continuous(breaks = seq(from = 0, to = 60000, by = 10000), limits = c(0, 60000), expand = expansion(mult = c(0, 0.05))) + 
  labs(
    x = NULL,
    y = "Deforestation (Ha)") +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_1_10a_plot

save_graphic(FSR_4_1_10a_plot, '4.3.3b', ' Area of deforestation associated with UK consumption of food commodities annually')
  save_csv(FSR_4_1_10a, '4.3.3b', '  Area of deforestation associated with UK consumption of food commodities annually')


# Predicted regional species loss----------------------------------------------- 
  
FSR_4_3_3d <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/Speciesloss.csv")

FSR_4_3_3d_plot <-  ggplot(FSR_4_3_3d, aes(x = Year, y = `Species loss`)) +
  geom_line(color = af_colours()[1]) +
  scale_x_continuous(breaks = seq(min(FSR_4_3_3d$Year), max(FSR_4_3_3d$Year), by = 2)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20), limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = "Predicted regional species loss (number)") +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_3_3d_plot

save_graphic(FSR_4_3_3d_plot, '4.3.3d', 'Predicted regional species loss associated with UK consumption of food commodities annually')
  save_csv(FSR_4_3_3d, '4.3.3d', 'Predicted regional species loss associated with UK consumption of food commodities annually')


# Scarcity-weighted blue water use ---------------------------------------------
  
FSR_4_1_10c <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/Scarcity weighted blue water.csv")

FSR_4_1_10c_plot <-  ggplot(FSR_4_1_10c, aes(x = Year, y = `Scarcity-weighted blue water use`)) +
  geom_line(color = af_colours()[1]) +
  scale_x_continuous(breaks = seq(min(FSR_4_1_10c$Year), max(FSR_4_1_10c$Year), by = 2)) +
  scale_y_continuous(breaks = seq(from = 0, to = 750, by = 100), limits = c(0, 750), expand = expansion(mult = c(0, 0.05))) +  
  labs(
    x = NULL,
    y = "Scarcity-weighted blue water use (billion cubic metres)") +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_1_10c_plot

save_graphic(FSR_4_1_10c_plot, '4.3.3c', 'Scarcity-weighted blue water use associated with UK consumption of food commodities annually') 
  save_csv(FSR_4_1_10c, '4.3.3c', 'Scarcity-weighted blue water use associated with UK consumption of food commodities annually')
