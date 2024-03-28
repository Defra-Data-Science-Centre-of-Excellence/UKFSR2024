library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

# There are 2 options for energy price data: 
#   price for fuels used in manufacturing
#   gas and electricity prices in the non-domestic sector
# Both available at https://www.gov.uk/government/collections/industrial-energy-prices

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



mfg_fuel |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = fuel), lwd = 1) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "pence per kilowatt hour") +
  theme_ukfsr(base_family = "GDS Transport Website")
