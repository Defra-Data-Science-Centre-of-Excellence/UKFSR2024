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

#setwd("~/UKFSR/theme 2/files")

source(here::here("utils", "load-font.R"))

vegetable_supply <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_2/input_data/t2_1_4/2_1_4e_vegetable_supply.csv")

#load data
#vegetable_supply <- read_csv("2_1_4_vegetable_supply.csv")

#filter data
vegetable_supply1<-vegetable_supply %>%
  group_by(Supply) %>%
  filter(Supply %in% c("Total production (minus exports)", "Imports from the EU", "Imports from the rest of the world"))

#transform wide to long data
vegetable_supply2 <- vegetable_supply1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = !Supply, names_to = "year", values_to = "percentage") |> 
  filter(year>"2002") |>
  mutate(Supply=case_when(Supply=="Total production (minus exports)" ~ "UK production", Supply=="Imports from the EU" ~ "Imports from EU", Supply=="Imports from the rest of the world" ~ "Imports from non-EU"))

vegetable_supply2$Supply <- as.factor(vegetable_supply2$Supply)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(vegetable_supply2$Supply)

#produce chart
vegetable_supply_chart <- vegetable_supply2 |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage/100, group = Supply, colour = Supply), lwd = 1) +
  #scale_y_continuous(limits = c(0,110)) +
  #scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100)) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
  labs(x = NULL,
       y = NULL)
  
  vegetable_supply_chart


#new file saves
save_graphic(vegetable_supply_chart, "2.1.4e", "vegetable supply")
save_csv(vegetable_supply2, "2.1.4e", "vegetable supply")

#save graphic
#ggsave(filename = "2_1_4e_vegetable_supply.svg",
#vegetable_supply_chart,
#width = 960,
#height = 640,
#units = "px",
#dpi = 72)