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
library(stringr)

#source(here::here("utils", "load-font.R"))

setwd("~/UKFSR/theme 2/files")

source("load-font.R")

grain_prod_type <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_2/2_1_2c_domestic_grain_production_by_type.csv")

#load data
#grain_prod_type <- read_csv("2_1_2_domestic_grain_production_by_type.csv")
#grain <- fread("2_1_2_domestic_grain_production.csv")

#filter data
grain_prod_type1<-grain_prod_type %>%
  group_by(Production) %>%
  filter(Production %in% c("Volume of harvested production (million tonnes) - total cereals", "Volume of harvested production (million tonnes) - wheat", "Volume of harvested production (million tonnes) - barley", "Volume of harvested production (million tonnes) - oats")) %>%
  mutate(Production=factor(Production, levels=c("Volume of harvested production (million tonnes) - total cereals", "Volume of harvested production (million tonnes) - wheat", "Volume of harvested production (million tonnes) - barley", "Volume of harvested production (million tonnes) - oats"), labels=c("All", "Wheat", "Barley", "Oats")))

#transform wide to long data
grain_prod_type2 <- grain_prod_type1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "million tonnes") |> 
  pivot_longer(cols = !Production, names_to = "year", values_to = "million tonnes") |> 
  filter(year>"2002")
  #mutate(year = factor(year, 
                       #levels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
                       #labels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")))# |>
  
grain_prod_type2$Production <- as.factor(grain_prod_type2$Production)

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(grain_prod_type2$Production)

  #ggplot chart production
grain_chart_prod_type <- grain_prod_type2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `million tonnes`, group = Production, colour = Production), lwd = 1) +
  scale_y_continuous(breaks = seq(0, 30, 5),limits = c(0, 30)) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 4)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million tonnes")
  
  grain_chart_prod_type

save_graphic(grain_chart_prod_type, "2.1.2c", "domestic grain by type")
save_csv(grain_prod_type, "2.1.2c", "domestic grain by type")

#ggsave(filename = "2_1_2c_domestic_grain_prod_by_type.svg",
       #grain_chart_prod_type,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)