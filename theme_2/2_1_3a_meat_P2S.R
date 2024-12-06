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

source("load-font.R")

meat <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_3/2_1_3a_livestock_p2s.csv")

#setwd("~/UKFSR/theme 2/files")

#meat <- read_csv("2_1_3_meat.csv")
#meat <- fread("2_1_3_meat.csv")

#meat <- meat %>%  row_to_names(row_number = 1)

#meat1 <- meat |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
#meat1<-meat %>%
  #group_by(Production_to_supply_ratio) %>%
  #filter(Production_to_supply_ratio %in% c("Beef", "Pork", "Poultry", "Mutton/Lamb", "Milk", "Eggs"))

#transform wide to long data
meat1 <- meat |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = !`Production_to_supply_ratio`, names_to = "year", values_to = "percentage") |> 
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
meat1$`Production_to_supply_ratio` <- as.factor(meat1$`Production_to_supply_ratio`)

af_categorical_colours <- afcolours::af_colours("categorical", n = 6)
names(af_categorical_colours)=levels(meat1$`Production_to_supply_ratio`)

  #filter(year>"2013") |>
meat_chart_P2S <- meat1 |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage/100, group = `Production_to_supply_ratio`, colour = `Production_to_supply_ratio`), lwd = 1) +
  #scale_y_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,1.2), labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 6)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = NULL)
  
  meat_chart_P2S

save_graphic(meat_chart_P2S, "2.1.3a", "Meat Prod-to-supply")
save_csv(meat, "2.1.3a", "Meat Prod-to-supply")

#ggsave(filename = "2_1_3a_meat_P2S.svg",
       #meat_chart_P2S,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)