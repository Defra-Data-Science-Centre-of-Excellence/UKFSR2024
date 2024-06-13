

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


# 3_1_2a_aggregate_energy_demand_agric_food_drink_manufact

FSR_3_1 <- aws.s3::s3read_using(FUN = readr::read_csv,
                          bucket = "s3-ranch-054",
                          object = "theme_3/input_data/3_1_2a_aggregate_energy_demand_agric_food_drink_manufact.csv")



F3_1a <- FSR_3_1 %>%
  filter(Year >= 2002) %>%
  gather(key,value, `Agriculture`, `Food and drink manufacturing`)  %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 
  


F3_1a_plot <- ggplot(F3_1a, aes(x=Year, y=value, colour=key, group=key)) +
  geom_line() +
  #scale_y_continuous(limits = c(0,4500), breaks=seq(0,4500,500)) +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = "Year",
       y = "Thousand tonnes oil equivalent") +
  scale_x_date(breaks=seq(as.Date("2002-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),labels=date_format("%Y"))+
  theme_ukfsr(base_family = "GDS Transport Website") 


F3_1a_plot

save_graphic(F3_1a_plot, '3.1.2a', ' Aggregate energy demand for agriculture and food and drink manufacturing') + 
  save_csv(F3_1a, '3.1.2a', ' Aggregate energy demand for agriculture and food and drink manufacturing')


------------------------------------------------------------------------------------------------------------------------------------------------


# 3.1.2b - Energy demand by energy type in the food and drink manufacturing sector
  


FSR_3_2 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_3/input_data/3_1_2b_energy_demand_food_drink_manufact_energy_type.csv")


F3_2 <- FSR_3_2 %>%
  filter(Year >= 2002) %>%
  gather(variable,value, `Coal`,`Petroleum products`,`Natural gas`,`Electricity`)  %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

F3_2_plot <- ggplot(F3_2, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = "Year",
       y = "Thousand tonnes oil equivalent") +
  scale_x_date(breaks=seq(as.Date("2002-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),labels=date_format("%Y"))+
  theme_ukfsr(base_family = "GDS Transport Website") 


F3_2_plot

save_graphic(F3_2_plot, '3.1.2b', '  Energy demand by energy type in the food and drink manufacturing sector') + 
  save_csv(F3_2, '3.1.2b', ' Energy demand by energy type in the food and drink manufacturing sector')

--------------------------------------------------------------------------------------------------------------------------------------------------
  
# 3.1.2c Energy demand by energy type in the agriculture sector
  
library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_3_3 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_3/input_data/3_1_2c_energy_demand_agric_energy_type.csv")



F3_3 <- FSR_3_3 %>%
  filter(Year >= 2002) %>%
  gather(variable,value, `Coal`,`Petroleum products`,`Natural gas`,`Bioenergy & waste`,`Electricity`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

F3_3_plot <- ggplot(F3_3, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = "Year",
       y = "Thousand tonnes oil equivalent") +
  scale_x_date(breaks=seq(as.Date("2002-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),labels=date_format("%Y"))+
  theme_ukfsr(base_family = "GDS Transport Website")

F3_3_plot

save_graphic(F3_3_plot, '3.1.2c', '  Energy demand by energy type in the agriculture sector') + 
  save_csv(F3_3, '3.1.2c', ' Energy demand by energy type in the agriculture sector')

------------------------------------------------------------------------------------------------------------------------------------------------
  
  
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# 3.1.2d: energy  price

ndcolspec <- cols(
  year = col_double(),
  quarter = col_double(),
  electricity = col_double(),
  gas = col_double()
)

nd_fuel <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_fsi/tfsi_5_1/output/csv/fsi_5_1_non_domestic_fuel_price_index.csv",
                                col_types = ndcolspec)

nd_fuel <- nd_fuel |> 
  pivot_longer(cols = c(-year, -quarter), names_to = "input") |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01"))) |> 
  select(date, value, input)

chart <- nd_fuel |> 
  filter(date >= "2014-01-01") |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = input)) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "index (2020 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")

chart

save_graphic(chart, '3.1.2d', '  Energy prices') + 
  save_csv(nd_fuel, '3.1.2d', ' Energy prices')
