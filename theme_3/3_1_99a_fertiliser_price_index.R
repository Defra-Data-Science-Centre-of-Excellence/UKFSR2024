library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

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

