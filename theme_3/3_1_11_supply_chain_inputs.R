library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)

source(here::here("utils", "load-font.R"))

# CO2 --------------------------------------------------------------------------

# Original data table from uktradeinfo
# 
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=d39d4559-a1a5-4204-a201-b90496ef63ca

co2 <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/3_1_11a_co2_trade_data.csv")


co2_out <- co2 |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

co2_cht <- co2_out |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(co2_out, "3.1.11a", "CO2 trade")
save_graphic(co2_cht, "3.1.11a", "CO2 trade")

co2_net <- co2_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)
 
co2_net_cht <-   ggplot(co2_net) +
  geom_col(aes(x = year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(co2_net, "3.1.11a", "CO2 net trade")
save_graphic(co2_net_cht, "3.1.11a", "CO2 net trade")


co2_countries <- co2 |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  filter(flow == "Imports", year >= 2023) |> 
  group_by(year, country) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

co2_countries |> 
  mutate(country = case_when(value <= 1 ~ "Other", .default = country)) |> 
  group_by(year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = fct_reorder(country, value) ), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")


# Hypochlorite -----------------------------------------------------------------

# Original data table from uktradeinfo
# 
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=1b7a1a0f-2802-46c9-a1c1-93d8d913d4e6


hypo <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/3_1_11b_hypochlorite_trade_data.csv")

hypo_out <- hypo |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000)


hypo_cht <- hypo_out |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(hypo_out, "3.1.11b", "hypochlorite trade")
save_graphic(hypo_cht, "3.1.11b", "hypochlorite trade")


hypo_net <- hypo_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

hypo_net_cht <-   ggplot(hypo_net) +
  geom_col(aes(x = year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(hypo_net, "3.1.11b", "hypochlorite net trade")
save_graphic(hypo_net_cht, "3.1.11b", "hypochlorite net trade")

hypo_countries <- hypo |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  filter(flow == "Imports", year >= 2023) |> 
  group_by(year, country) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

hypo_countries |> 
  mutate(country = case_when(value <=1 ~ "Other", .default = country)) |> 
  group_by(year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = fct_reorder(country, value)), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")



# PET --------------------------------------------------------------------------
# https://www.trade-tariff.service.gov.uk/commodities/3915902000
# https://www.trade-tariff.service.gov.uk/commodities/3926909790
# 
# uktradeinfo table
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=62cfd6e7-7906-4486-9fe6-af75b925d420

pet <- aws.s3::s3read_using(FUN = read_csv,
                             bucket = ukfsr::s3_bucket(),
                             object = "theme_3/input_data/3_1_11c_pet_trade_data.csv")

pet_out <- pet |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(date_hierarchy_year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000)
  
pet_cht <- pet_out |> 
  ggplot() +
  geom_col(aes(x = date_hierarchy_year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")  

save_csv(pet_out, "3.1.11c", "pet trade")
save_graphic(pet_cht, "3.1.11c", "pet trade")


pet_net <- pet_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

pet_net_cht <-   ggplot(pet_net) +
  geom_col(aes(x = date_hierarchy_year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(pet_net, "3.1.11c", "pet net trade")
save_graphic(pet_net_cht, "3.1.11c", "pet net trade")


pet_countries <- pet |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  filter(flow == "Imports", date_hierarchy_year >= 2023) |> 
  group_by(date_hierarchy_year, country_hierarchy_country) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

pet_countries |> 
  mutate(country = case_when(value <= 15 ~ "Other", .default = country_hierarchy_country)) |> 
  group_by(date_hierarchy_year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = date_hierarchy_year, y = value, fill = fct_reorder(country, value) ), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")


