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

fruit_supply <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_2/input_data/t2_1_4/2_1_4f_fruit_supply.csv")

#load data
#fruit_supply <- read_csv("2_1_4_fruit_supply.csv")

#filter data
#livestock1<-livestock %>%
  #group_by(Production_to_supply_ratio) %>%
  #filter(Production_to_supply_ratio %in% c("Beef", "Pork", "Lamb", "Poultry", "Milk", "Eggs"))

#transform wide to long data
fruit_supply1 <- fruit_supply |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = !Supply, names_to = "year", values_to = "percentage") |> 
  filter(year>"2002") |>
  mutate(Supply=case_when(Supply=="Total production (minus exports)" ~ "UK production", Supply=="Imports from the EU" ~ "Imports from EU", Supply=="Imports from the rest of the world" ~ "Imports from non-EU"))
  
fruit_supply1$Supply <- as.factor(fruit_supply1$Supply)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(fruit_supply1$Supply)

  #produce chart
fruit_supply_chart <- fruit_supply1 |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage, group = Supply, colour = Supply), lwd = 1) +
  #scale_y_continuous(limits = c(0,110)) +
  scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
  labs(x = NULL,
       y = "Percentage")
  
  fruit_supply_chart

#new file saves  
save_graphic(fruit_supply_chart, "2.1.4f", "fruit supply")
save_csv(fruit_supply1, "2.1.4f", "fruit supply")

  #save graphic
#ggsave(filename = "2_1_4g_fruit_supply.svg",
       #fruit_supply_chart,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)