library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# from https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/bulletins/businessdemography/latest

data <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_3/input_data/business_demography.csv")

cht <- data |> 
  ggplot() +
  geom_col(aes(x = year, y = percent, fill = event), position = position_dodge()) + 
  scale_x_continuous(breaks = seq(2017, 2022, by = 1)) +
  scale_y_continuous(limits = c(0,15), labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE)

save_graphic(cht, "3.4.2b", "fdm business demography")
save_csv(data, "3.4.2b", "fdm business demography")
