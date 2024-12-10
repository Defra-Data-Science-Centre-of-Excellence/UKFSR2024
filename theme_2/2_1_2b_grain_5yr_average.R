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

setwd("~/UKFSR/theme 2/files")

source("load-font.R")

grain_average <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_2/input_data/t2_1_2/2_1_2a_b_domestic_grain_production.csv")

#grain_average <- read_csv("2_1_2_domestic_grain_production.csv")
#grain <- fread("2_1_2_domestic_grain_production.csv")

#grain <- grain %>%  row_to_names(row_number = 1)

#grain1 <- grain |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
grain_average1<-grain_average %>%
  group_by(Production) %>%
  filter(Production %in% c("Production (mt)", "Total domestic uses (mt)", "Production (mt)(5yr average)")) %>%
  mutate(Production=factor(Production, levels=c("Production (mt)", "Total domestic uses (mt)", "Production (mt)(5yr average)"), labels=c("Cereal production", "Domestic use", "5 year average production")))

#transform wide to long data
grain_average2 <- grain_average1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "th. tonnes") |> 
  pivot_longer(cols = !Production, names_to = "year", values_to = "million tonnes") |> 
  filter(year>"2002")
  #mutate(year = factor(year, 
                       #levels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
                       #labels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")))# |>
  
grain_average2$Production <- as.factor(grain_average2$Production)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(grain_average2$Production)

  #filter(year>"2013") |>
grain_chart_average <- grain_average2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `million tonnes`, group = Production, colour = Production), lwd = 1) +
  #scale_y_continuous(limits = c(0,30000)) +
  scale_y_continuous(breaks = seq(0, 30, 5),limits = c(0, 30), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million tonnes")
  
  grain_chart_average

save_graphic(grain_chart_average, "2.1.2b", "domestic grain supply average")
save_csv(grain, "2.1.2b", "domestic grain supply average")

#ggsave(filename = "2_1_2b_domestic_grain_average.svg",
       #grain_chart_average,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)