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

#setwd("~/UKFSR/theme 2/files")

source("load-font.R")

vegetables <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_4/2_1_4a_c_d_fresh_fruit_and_vegetables.csv")

#vegetables <- read_csv("2_1_8_fresh_fruit_and_vegetables.csv")

#filter data
vegetables1<-vegetables %>%
  group_by(`Supply and use 2`) %>%
  filter(`Supply and use 2` %in% c("Total production FV", "Imports FV", "Exports FV")) %>%
  mutate(`Supply and use 2`=case_when(`Supply and use 2`=="Total production FV" ~ "UK production", `Supply and use 2`=="Imports FV" ~ "Imports", `Supply and use 2`=="Exports FV" ~ "Exports")) %>%
  mutate(usage=factor(`Supply and use 2`, levels=c("UK production", "Imports", "Exports")))

vegetables1$"Supply and use 2" <- as.factor(vegetables1$"Supply and use 2")

#transform wide to long data
vegetables2 <- vegetables1 |> 
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
  
vegetables2$`Supply and use 2` <- as.factor(vegetables2$`Supply and use 2`)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(vegetables2$`Supply and use 2`)

  #filter(year>"2002") |>
vegetable_chart_prod <- vegetables2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `m.tonnes`, group = `Supply and use 2`, colour = `Supply and use 2`), lwd = 1) +
  #scale_y_continuous(limits = c(0,3000)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5),limits = c(0, 3)) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million tonnes")
  
  vegetable_chart_prod

save_graphic(vegetable_chart_prod, "2.1.4c", "vegetable availability")
save_csv(vegetables2, "2.1.4c", "vegetable availability")

#ggsave(filename = "2_1_4c_vegetable_availability.svg",
       #vegetable_chart_prod,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)