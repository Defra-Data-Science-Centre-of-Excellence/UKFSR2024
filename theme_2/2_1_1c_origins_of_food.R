library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(data.table)
library(janitor)

setwd("~/UKFSR/theme 2/files")

source(here::here("utils", "load-font.R"))

source("load-font.R")

oof <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_1/2_1_1c_origins_of_food.csv")
                            
#oof <- read_csv("2_1_1_origins_of_food.csv")
#oof <- fread("2_1_1_origins_of_food.csv")

#oof <- oof %>%  row_to_names(row_number = 1)

oof1 <- oof |> 
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "perc") |> 
  pivot_longer(cols = !Type, names_to = "year", values_to = "perc") |> 
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
oof1$Type <- as.factor(oof1$Type)

af_categorical_colours <- afcolours::af_colours("categorical", n = 6)
names(af_categorical_colours)=levels(oof1$Type)

  #filter(year>"2013") |>
oof_chart <- oof1 |> 
  ggplot() +
  geom_line(aes(x = year, y = perc/100, group = Type, colour = Type), lwd = 1) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 6)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(10,20,10,10,unit = "pt")) +
  labs(x = NULL,
       y = NULL)
  
  oof_chart

save_graphic(oof_chart, "2.1.1c", "origins of food")
save_csv(oof, "2.1.1c", "origins of food")

#ggsave(filename = "2_1_1c_origins_of_food.svg",
       #oof_chart,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)