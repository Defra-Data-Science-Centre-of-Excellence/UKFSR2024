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

#source(here::here("utils", "load-font.R"))

setwd("~/UKFSR/theme 2/files")

source("load-font.R")

grain <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_2/2_1_2a_b_domestic_grain_production.csv")

#grain <- read_csv("2_1_2_domestic_grain_production.csv")
#grain <- fread("2_1_2_domestic_grain_production.csv")

#grain <- grain %>%  row_to_names(row_number = 1)

#grain1 <- grain |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
grain1<-grain %>%
  group_by(Production) %>%
  filter(Production %in% c("Production as % of total new supply for use in UK"))

#transform wide to long data
grain2 <- grain1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = !Production, names_to = "year", values_to = "percentage") |> 
  filter(year>"2002")
  #mutate(year = factor(year, 
                       #levels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
                       #labels = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")))# |>
  
grain2$Production <- as.factor(grain2$Production)

af_categorical_colours <- afcolours::af_colours("categorical", n = 1)
names(af_categorical_colours)=levels(grain2$Production)

  #filter(year>"2013") |>
grain_chart_P2S <- grain2 |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage/100, group = Production, colour = Production), lwd = 1) +
  #scale_y_continuous(limits = c(0,110)) +
  scale_y_continuous(limits = c(0,1.2), labels = scales::percent) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 1)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(20,20,5,5,unit = "pt")) +
  labs(x = NULL,
       y = NULL)
  
  grain_chart_P2S

save_graphic(grain_chart_P2S, "2.1.2a", "domestic grain supply")
save_csv(grain, "2.1.2a", "domestic grain supply")

#ggsave(filename = "2_1_2a_grain_P2S.svg",
       #grain_chart_P2S,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)