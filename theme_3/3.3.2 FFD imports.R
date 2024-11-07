library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)
library(zoo)

source(here::here("utils", "load-font.R"))

import_vol <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_3/input_data/ffd_imports_volume.csv") |> 
  mutate(area = factor(area, levels = c("eu", "non eu"), labels = c("EU", "Non EU")))

chart <- 
import_vol |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = area)) +
  geom_vline(xintercept = 2020.5, linetype = "dashed") +
  annotate("text", x = 2020.3, y = 3e+10, label = "End of \n transition period", hjust = "right", size = 8, family = "GDS Transport Website") +
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) +
  scale_y_continuous(labels = scales::label_number(scale = 0.000000001), breaks = seq(0,3e+10, by = 0.6e+10), limits = c(0, 3e+10)) +
  scale_colour_manual(values = af_colours("duo")) +
  labs(x = NULL, y = "Million tonnes") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(chart, "3.3.2a", "annual import volumes of ffd")
save_csv(import_vol, "3.3.2a", "annual import volumes of ffd")

# NOT USED import values -------------------------------------------------------
import_data <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/ffd_imports.csv") |> 
  pivot_longer(cols = -year, names_to = "category") |> 
  mutate(status = case_when(stringr::str_detect(category, "real") ~ "Real",stringr::str_detect(category, "nominal") ~ "Nominal"),
         area = case_when(stringr::str_starts(category, "non_eu") ~ "Non EU",stringr::str_starts(category, "eu") ~ "EU"))

chart <- 
import_data |> ggplot() + geom_line(aes(x = year, y = value, colour = area, linetype = status)) +
  geom_vline(xintercept = 2020.5, linetype = "dashed") +
  annotate("text", x = 2020.3, y = 4.2e+10, label = "End of \n transition period", hjust = "right", size = 8, family = "GDS Transport Website") +
  scale_y_continuous(labels = scales::label_comma(scale = 0.000000001, prefix = "£", suffix = "bn")) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) +
  scale_colour_manual(values = af_colours("duo")) +
  labs(x = NULL, y = NULL, colour = "merge") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(chart, "3.3.2a", "annual imports of ffd")
save_csv(import_data, "3.3.2a", "annual imports of ffd")

# ggplot(import_data, aes(x = year, y = value)) +
#   geom_line(data = filter(import_data, category == "eu_nominal"), colour = af_colours("duo")[1]) +
#   geom_line(data = filter(import_data, category == "eu_real"), colour = af_colours("duo")[1], linetype = "dashed") +
#   geom_line(data = filter(import_data, category == "non_eu_nominal"), colour = af_colours("duo")[2]) +
#   geom_line(data = filter(import_data, category == "non_eu_real"), colour = af_colours("duo")[2], linetype = "dashed") +
#   geom_vline(xintercept = 2020.5, linetype = "dashed") +
#   annotate("text", x = 2020.3, y = 4.2e+10, label = "End of \n transition period", hjust = "right", size = 8) +
#   scale_y_continuous(labels = scales::label_comma(scale = 0.000000001, prefix = "£", suffix = "bn")) +
#   labs(x = NULL, y = NULL) +
#   theme_ukfsr(base_family = "GDS Transport Website")