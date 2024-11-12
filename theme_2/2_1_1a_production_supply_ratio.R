library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(data.table)

#source(here::here("utils", "load-font.R"))

setwd("~/UKFSR/theme 2/files")

source("load-font.R")

psp <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_1/2_1_1a_production_to_supply.csv")

#psp <- fread("2_1_1_production_to_supply.csv")

psp_chart <- psp |> 
  pivot_longer(cols = all_food:indigenous_food, names_to = "type") |> 
  mutate(type = factor(type, 
                       levels = c("all_food", "indigenous_food"),
                       labels = c("all food", "Indigenous food"))) |> 
  filter(year>2002) |>
  ggplot() +
  geom_line(aes(x = year, y = value/100, colour = type), lwd = 1) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(breaks = seq(2003, 2023, 5),limits = c(2003, 2023)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = NULL)
  
  psp_chart

save_graphic(psp_chart, "2.1.1a", "production supply ratio")
save_csv(psp, "2.1.1a", "production supply ratio")

#ggsave(filename = "2_1_1a_production_to_supply_ratio.svg",
       #psp_chart,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)