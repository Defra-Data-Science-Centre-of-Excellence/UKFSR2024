library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

psp <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/t2_1_1/output/csv/2_1_1_production_supply_ratio.csv")


psp_chart <- psp |> 
  pivot_longer(cols = total_food:indigenous_food, names_to = "type") |> 
  mutate(type = factor(type, 
                       levels = c("total_food", "indigenous_food"),
                       labels = c("All food", "Indigenous food"))) |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = type), lwd = 1) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")
  

save_graphic(psp_chart, "2.1.1", "production supply ratio")