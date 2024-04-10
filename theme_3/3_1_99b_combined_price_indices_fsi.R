library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))


ndcolspec <- cols(
  year = col_double(),
  quarter = col_double(),
  electricity = col_double(),
  gas = col_double()
)

nd_fuel <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_99/output/csv/3_1_99b_non_domestic_fuel_price_index.csv",
                                col_types = ndcolspec)

nd_fuel <- nd_fuel |> 
  pivot_longer(cols = c(-year, -quarter), names_to = "input") |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01"))) |> 
  select(date, value, input)



fertiliser <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_3/t3_1_99/output/csv/3_1_99a_fertiliser_price_index.csv",
                                   col_types = cols(date = col_date(format = "%d/%m/%Y")))

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
  geom_line(aes(x = date, y = value, colour = input), lwd = 1) +
  scale_colour_manual(values = af_colours()) +
  labs(x = NULL,
       y = "index (2020 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")


save_graphic(chart, "3.1.99b", "combined price indices fsi")
