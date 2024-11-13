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

croppable_area <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_4/2_2_4b_croppable_area.csv")

#croppable_area <- read_csv("2_1_9_croppable_area.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
croppable_area1<-croppable_area %>%
  group_by(`Land use`) %>%
  filter(`Land use` %in% c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops")) %>%
  mutate(`Land use`=factor(`Land use`, levels=c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops"), labels=c("Permanent grassland", "Croppable area", "Arable crops", "Potatoes", "Oilseeds", "Horticultural crops")))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

#transform wide to long data
croppable_area2 <- croppable_area1 |> 
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "m.hectares") |> 
  pivot_longer(cols = !c("Land use"), names_to = "year", values_to = "m.hectares") |> 
  mutate(year=as.numeric(year)) |>
  filter(year>2002)
  #mutate(year = factor(year, 
                       #levels = c("2003":"2023"),
                       #labels = c("2003":"2023")))# |>
  
croppable_area2$"Land use" <- as.factor(croppable_area2$"Land use")

af_categorical_colours <- afcolours::af_colours("categorical", n = 6)
names(af_categorical_colours)=levels(croppable_area2$`Land use`)

  #filter(year>"2013") |>
croppable_area_chart <- croppable_area2 |> 
  ggplot() +
  geom_line(aes(x = year, y = m.hectares, group = `Land use`, colour = `Land use`), lwd = 1) +
  #scale_y_continuous(limits = c(0,600)) +
  scale_y_continuous(breaks = seq(0, 11000, 1000),limits = c(0, 11000)) +
  scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 6)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million hectares")
  
  croppable_area_chart

save_graphic(croppable_area_chart, "2.2.4b", "croppable area")
save_csv(croppable_area2, "2.2.4b", "croppable area")

#ggsave(filename = "2_2_4b_croppable_area.svg",
#       croppable_area_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)