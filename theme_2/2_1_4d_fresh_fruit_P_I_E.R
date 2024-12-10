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

source(here::here("utils", "load-font.R"))

#setwd("~/UKFSR/theme 2/files")

source("load-font.R")

fruit <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_2/input_data/t2_1_4/2_1_4a_c_d_fresh_fruit_and_vegetables.csv")

#fruit <- read_csv("2_1_8_fresh_fruit_and_vegetables.csv")

#filter data
fruit1<-fruit %>%
  group_by(`Supply and use 2`) %>%
  filter(`Supply and use 2` %in% c("Total production FF", "Imports FF", "Exports FF")) %>%
  mutate(`Supply and use 2`=case_when(`Supply and use 2`=="Total production FF" ~ "UK production", `Supply and use 2`=="Imports FF" ~ "Imports", `Supply and use 2`=="Exports FF" ~ "Exports")) %>%
  mutate(usage=factor(`Supply and use 2`, levels=c("UK production", "Imports", "Exports")))

fruit1$"Supply and use 2" <- as.factor(fruit1$"Supply and use 2")

#transform wide to long data
fruit2 <- fruit1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "th.tonnes") |> 
  pivot_longer(cols = 4:42, names_to = "year", values_to = "th.tonnes") |> 
  select(-Type, -`Supply and use 2`) |>
  mutate(`th.tonnes`= str_replace(`th.tonnes`, ",", "")) |>
  mutate(`th.tonnes`= as.numeric(`th.tonnes`)) |>
  mutate("m.tonnes"=th.tonnes/(1000)) |>
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
fruit2$`Supply and use 2` <- as.factor(fruit2$`Supply and use 2`)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(fruit2$`Supply and use 2`)

  #filter(year>"2002") |>
fruit_chart_prod <- fruit2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `m.tonnes`, group = `Supply and use 2`, colour = `Supply and use 2`), lwd = 1) +
  #scale_y_continuous(limits = c(0,4000)) +
  scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0, 4), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million tonnes")
  
  fruit_chart_prod

save_graphic(fruit_chart_prod, "2.1.4d", "fruit availability")
save_csv(fruit2, "2.1.4d", "fruit availability")

#ggsave(filename = "2_1_4d_fruit_availability.svg",
       #fruit_chart_prod,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)