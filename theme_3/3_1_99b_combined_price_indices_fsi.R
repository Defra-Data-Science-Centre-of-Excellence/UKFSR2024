library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# FSI Indicator 5: energy and fertiliser price indices--------------------------

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



fertiliser <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_fsi/tfsi_5_1/output/csv/fsi_5_1_fertiliser_price_index.csv")

fertiliser <- fertiliser |>
  mutate(input = "fertiliser")

data <- nd_fuel |> 
  bind_rows(fertiliser) |> 
  mutate(input = factor(input,
                       levels = c("electricity", "gas", "fertiliser"),
                       labels = c("Electricity", "Gas", "Fertiliser")))

chart <- data |> 
  filter(date >= "2014-01-01") |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = input)) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "index (2020 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")


save_graphic(chart, "fsi.5.1", "combined price indices fsi")





# OLD UNUSED CODE FOR ENERGY INDICATOR OPTIONS BELOW----------------------------

# There are 2 options for energy price data: 
#   price for fuels used in manufacturing
#   gas and electricity prices in the non-domestic sector
# Both available at https://www.gov.uk/government/collections/industrial-energy-prices
# Using here the figures excluding Climate Change Levy since that seems to be
# what DESNZ do in their publications

# Manufacturing fuel prices-----------------------------------------------------

# Available here: https://www.gov.uk/government/statistical-data-sets/prices-of-fuels-purchased-by-manufacturing-industry
# Using table 3.1.2


colspec <- cols(
  year = col_double(),
  quarter = col_double(),
  heavy_fuel_oil = col_double(),
  gas_oil = col_double(),
  electricity = col_double(),
  gas = col_double()
)

# Need to deal with revised data appearing as text
mfg_fuel <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_3/t3_1_4/output/csv/3_1_4b_fuel_prices_for_manufacturing.csv",
                                 col_types = colspec)

mfg_fuel <- mfg_fuel |> 
  pivot_longer(cols = c(-year, -quarter), names_to = "fuel") |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01")),
         fuel = factor(fuel,
                       levels = c("electricity", "gas", "gas_oil", "heavy_fuel_oil"),
                       labels = c("Electricity", "Gas", "Gas Oil", "Heavy Fuel Oil")))



mfg_chart <- mfg_fuel |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = fuel), lwd = 1) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "pence per kilowatt hour") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(mfg_chart, "3.1.4b", "fuel prices for manufacturing")


# Non-domestic prices ----------------------------------------------------------
# Available here: https://www.gov.uk/government/statistical-data-sets/gas-and-electricity-prices-in-the-non-domestic-sector
# Using table 3.4.1

ndcolspec <- cols(
  year = col_double(),
  quarter = col_double(),
  electricity = col_double(),
  gas = col_double()
)

nd_fuel <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_4/output/csv/3_1_4c_non_domestic_fuel_prices.csv",
                                col_types = ndcolspec)

nd_fuel <- nd_fuel |> 
  pivot_longer(cols = c(-year, -quarter), names_to = "fuel") |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01")),
         fuel = factor(fuel,
                       levels = c("electricity", "gas"),
                       labels = c("Electricity", "Gas")))

nd_chart <- nd_fuel |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = fuel), lwd = 1) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "pence per kilowatt hour") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(nd_chart, "3.1.4c", "non domestic fuel prices")

# OLD UNUSED CODE FOR FERTILISER PRICES BELOW-----------------------------------
# Defra agriculture price index
# https://www.gov.uk/government/collections/agricultural-price-indices

fertiliser <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_3/t3_1_99/output/csv/3_1_99a_fertiliser_price_index.csv",
                                   col_types = cols(date = col_date(format = "%d/%m/%Y")))


chart <- fertiliser |> 
  ggplot() +
  geom_line(aes(x = date, y = value), 
            colour = af_colours(type = "categorical", n = 1),
            lwd = 1) +
  labs(x = NULL,
       y = "index (2020 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")


save_graphic(chart, "3.1.99a", "fertiliser price index")

