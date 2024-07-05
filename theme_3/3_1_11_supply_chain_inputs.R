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
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=9985d218-7b76-4c40-bdf2-9bea6f6c7de5

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

# Hypochlorite -----------------------------------------------------------------

# Original data table from uktradeinfo
# 
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=8c477a4b-f736-4191-9ac7-45338de93cd7


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
