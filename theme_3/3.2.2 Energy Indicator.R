

#devtools::install_github("FoodchainStats/ukfsr")

library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)
library(zoo)

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")


# Aggregate energy demand ------------------------------------------------------

FSR_3_1_5 <- aws.s3::s3read_using(FUN = readr::read_csv,
                          bucket = "s3-ranch-054",
                          object = "theme_3/input_data/aggregate_energy_demand_agric_food_drink_manufact.csv")



FSR_3_1_5 <- FSR_3_1_5 %>%
  filter(Year >= 2009) %>%
  gather(key,value, `Agriculture`, `Food and drink manufacturing`)  %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 
  


FSR_3_1_5plot <- ggplot(FSR_3_1_5, aes(x=Year, y=value, colour=key, group=key)) +
  geom_line() +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_colour_manual(values = af_colours("categorical")) + 
  labs(x = NULL,
       y = "Thousand tonnes oil equivalent") +
  scale_x_date(breaks=seq(as.Date("2002-01-01"),Sys.Date()-lubridate::years(1),by = "3 year"),labels=scales::label_date(format = "%Y"))+
  theme_ukfsr(base_family = "GDS Transport Website") 


FSR_3_1_5plot

save_graphic(FSR_3_1_5plot, '3.2.2a', ' Aggregate energy demand for agriculture and food and drink manufacturing') + 
  save_csv(FSR_3_1_5, '3.2.2a', ' Aggregate energy demand for agriculture and food and drink manufacturing')


# Energy demand by type --------------------------------------------------------

FSR_3_1_5a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_3/input_data/energy_demand_food_drink_manufact_energy_type.csv")

FSR_3_1_5ab <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/energy_demand_agric_energy_type.csv")


FSR_3_1_5a <- FSR_3_1_5a %>%
  filter(Year >= 2009) %>%
  gather(variable,value, `Coal`,`Petroleum products`,`Natural gas`,`Electricity`)  %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

FSR_3_1_5ab <- FSR_3_1_5ab %>%
  filter(Year >= 2009) %>%
  gather(variable,value, `Coal`,`Petroleum products`,`Natural gas`,`Bioenergy & waste`,`Electricity`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 


# Combining the data for dual axis plot
combined_data <- bind_rows(
  FSR_3_1_5a %>% mutate(Sector = "Food and Drink Manufacturing"),
  FSR_3_1_5ab %>% mutate(Sector = "Agriculture")
)

# Dual Axis Line Graph (Side by Side)
dual_axis_plot_side_by_side <- ggplot(combined_data, aes(x=Year, y=value, colour=variable, group=interaction(variable, Sector))) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  scale_x_date(breaks=seq(as.Date("2002-01-01"),Sys.Date()-lubridate::years(1),by = "5 year"),labels=scales::label_date(format = "%Y"))+
  facet_wrap(~ Sector, nrow = 1) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL,
       y = "Thousand tonnes oil equivalent") +
  theme_ukfsr(base_family = "GDS Transport Website")

dual_axis_plot_side_by_side

# Save the dual axis plot
save_graphic(dual_axis_plot_side_by_side, '3.2.2b', 'Energy demand by energy type in food/drink manufacturing and agriculture sectors') + 
  save_csv(combined_data, '3.2.2b', 'Energy demand by energy type in food/drink manufacturing and agriculture sectors')


# ------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Support 2 - Energy demand by energy type in the food and drink manufacturing sector 
 
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# 3.1.2b: energy  price

ndcolspec <- cols(
  year = col_double(),
  quarter = col_double(),
  electricity = col_double(),
  gas = col_double()
)

nd_fuel <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/input_data/non_domestic_fuel_price_index.csv",
                                col_types = ndcolspec)

nd_fuel <- nd_fuel |> 
  pivot_longer(cols = c(-year, -quarter), names_to = "input") |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01")),
         input = factor(input, levels = c("electricity", "gas"), labels = c("Electricity", "Gas"))) |> 
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

save_graphic(chart, '3.2.2c', 'non domestic Energy price index') + 
  save_csv(nd_fuel, '3.2.2c', 'non domestic Energy price index')



