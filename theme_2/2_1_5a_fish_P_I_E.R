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

fish <- aws.s3::s3read_using(FUN = read_csv,
                                     bucket = ukfsr::s3_bucket(),
                                     object = "theme_2/input_data/t2_1_5/2_1_5a_fish.csv")

#setwd("~/UKFSR/theme 2/files")

#fish <- read_csv("2_1_5_fish.csv")

#filter data
fish1<-fish %>%
  group_by(Supply) %>%
  filter(Supply %in% c("Landings by UK vessels into the UK volume", "Imports volume", "Exports volume"))
  
  fish1$Supply <- as.factor(fish1$Supply)
  fish1$Units <- as.factor(fish1$Units)

#transform wide to long data
fish2 <- fish1 |> 
  select(-Units) |>
  #pivot_longer(cols = "2012":"2022", names_to = "year", values_to = "th. tonnes") |> 
  pivot_longer(cols = "2012":"2022", names_to = "year", values_to = "th. tonnes") |> 
  filter(year>"2012") |>
  mutate(year = factor(year, 
                       levels = c("2012":"2022"),
                       labels = c("2012":"2022"))) |>
  mutate(Supply=case_when(Supply=="Imports volume" ~ "Imports", Supply=="Exports volume" ~ "Exports", Supply=="Landings by UK vessels into the UK volume" ~ "UK landings")) %>%
  mutate(Supply=factor(Supply, levels=c("Imports", "Exports", "UK landings")))
  
#fish2$Supply <- as.factor(fish2$Supply)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(fish2$Supply)

  #filter(year>"2013") |>
fish_chart_prod <- fish2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `th. tonnes`, group = Supply, colour = Supply), lwd = 1) +
  #scale_y_continuous(limits = c(0,800)) +
  scale_y_continuous(breaks = seq(0, 800, 100),limits = c(0, 800), expand = expansion(mult = c(0,0.05))) +
  scale_x_discrete(breaks = c(2014, 2016, 2018, 2020, 2022)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Thousand tonnes")
  
  fish_chart_prod

save_graphic(fish_chart_prod, "2.1.5a", "Fish supply")
save_csv(fish2, "2.1.5a", "Fish supply")

#ggsave(filename = "2_1_5a_fish_supply.svg",
       #fish_chart_prod,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)