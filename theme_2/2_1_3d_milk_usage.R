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

#setwd("~/UKFSR/theme 2/files")

source("load-font.R")

milk_usage <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_3/2_1_3d_milk_usage.csv")

#read data into environment
#milk_usage <- read_csv("2_1_3_milk_usage.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#transform wide to long data
milk_usage1 <- milk_usage |> 
  #pivot_longer(cols = "2003":"2023", names_to = "usage", values_to = "m.hectares") |> 
  pivot_longer(cols = !c("Date"), names_to = "usage", values_to = "m.litres") #|> 
  #mutate(year=as.numeric(year)) |>
  #filter(year>2002)
#mutate(year = factor(year, 
#levels = c("2003":"2023"),
#labels = c("2003":"2023")))# |>

#milk_usage1$"usage" <- as.factor(milk_usage1$"usage")

#filter data
milk_usage2<-milk_usage1 %>%
  #group_by("usage") %>%
  filter(usage %in% c("Disposals of milk for Liquid milk (million litres)", "Total disposals of milk for cheese (million litres)", "Disposals of milk for Milk Powders (million litres)", "Other")) %>%
  filter(!is.na(m.litres))%>%
  mutate(usage=case_when(usage=="Disposals of milk for Liquid milk (million litres)" ~ "Liquid milk", usage=="Total disposals of milk for cheese (million litres)" ~ "Cheese", usage=="Disposals of milk for Milk Powders (million litres)" ~ "Milk powders", usage=="Other" ~ "Other dairy products")) %>%
  mutate(usage=factor(usage, levels=c("Liquid milk", "Cheese", "Milk powders", "Other dairy products"))) %>%
  mutate("b.litres"=m.litres/(1000))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(milk_usage2$usage)

  #filter(year>"2013") |>
milk_usage_chart <- milk_usage2 |> 
  ggplot() +
  geom_line(aes(x = Date, y = b.litres, group = usage, colour = usage), lwd = 1) +
  #scale_y_continuous(limits = c(0,600)) +
  scale_y_continuous(breaks = seq(0, 7.0, 1),limits = c(0, 7.0)) +
  #scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 4)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Billion litres")
  
  milk_usage_chart

save_graphic(milk_usage_chart, "2.1.3d", "milk usage")
save_csv(milk_usage2, "2.1.3d", "milk usage")

#ggsave(filename = "2_1_3d_milk_usage.svg",
       #milk_usage_chart,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)