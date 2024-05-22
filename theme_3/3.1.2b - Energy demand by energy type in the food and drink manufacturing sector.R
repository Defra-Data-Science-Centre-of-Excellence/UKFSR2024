#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_3_2 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_3/input_data/3_1_2b_energy_demand_food_drink_manufact_energy_type.csv")


F3_2 <- FSR_3_2 %>%
  gather(variable,value, `Coal`,`Petroleum products`,`Natural gas`,`Electricity`)  %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

F3_2_plot <- ggplot(F3_2, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = "Year",
       y = "Thousand tonnes oil equivalent") +
  scale_x_date(breaks=seq(as.Date("1998-01-01"),Sys.Date()-lubridate::years(1),by = "3 year"),labels=date_format("%Y"))+
  theme_ukfsr(base_family = "GDS Transport Website") 


F3_2_plot

save_graphic(F3_2_plot, '3.1.2b', '  Energy demand by energy type in the food and drink manufacturing sector') + 
  save_csv(F3_2, '3.1.2b', ' Energy demand by energy type in the food and drink manufacturing sector')
