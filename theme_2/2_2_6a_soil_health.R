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

source(here::here("utils", "load-font.R"))

#setwd("~/UKFSR/theme 2/files")

#source("load-font.R")

nutrient_balance <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_6/2_2_6a_soil_health.csv")

#nutrient_balance <- read_csv("2_2_6_soil_health.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
#croppable_area1<-croppable_area %>%
  #group_by(`Land use`) %>%
  #filter(`Land use` %in% c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops")) %>%
  #mutate(`Land use`=factor(`Land use`, levels=c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops"), labels=c("Permanent grassland", "Croppable area", "Arable crops", "Potatoes", "Oilseeds", "Horticultural crops")))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

#transform wide to long data
nutrient_balance1 <- nutrient_balance |> 
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "kg_per_ha") |> 
  pivot_longer(cols = !c("soil_nutrient_balance_kg_per_ha"), names_to = "year", values_to = "kg_per_ha") |> 
  mutate(year=as.numeric(year)) |>
  filter(year>2008)
  #mutate(year = factor(year, 
                       #levels = c("2003":"2023"),
                       #labels = c("2003":"2023")))# |>
  
nutrient_balance1$"soil_nutrient_balance_kg_per_ha" <- as.factor(nutrient_balance1$"soil_nutrient_balance_kg_per_ha")

af_categorical_colours <- afcolours::af_colours("categorical", n = 2)
names(af_categorical_colours)=levels(nutrient_balance1$`soil_nutrient_balance_kg_per_ha`)

  #filter(year>"2013") |>
nutrient_balance_chart <- nutrient_balance1 |> 
  ggplot() +
  geom_line(aes(x = year, y = kg_per_ha, group = `soil_nutrient_balance_kg_per_ha`, colour = `soil_nutrient_balance_kg_per_ha`), lwd = 1) +
  #scale_y_continuous(limits = c(0,600)) +
  scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100)) +
  #scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)) +
  scale_colour_manual(values = af_colours("categorical", n = 2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "kg per hectare")
  
nutrient_balance_chart

save_graphic(nutrient_balance_chart, "2.2.6a", "nutrient balance")
save_csv(nutrient_balance1, "2.2.6a", "nutrient balance")

#ggsave(filename = "2_2_6a_nutrient_balance.svg",
#       nutrient_balance_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)