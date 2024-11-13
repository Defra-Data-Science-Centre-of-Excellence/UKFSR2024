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

#source("load-font.R")
source(here::here("utils", "load-font.R"))

methane <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_8/2_2_8c_methane.csv",skip=2)

#load data
#methane <- read_csv("2_2_2_methane_nitrous_oxide.csv")

#filter data
methane1<-methane %>%
  group_by(Emissions) %>%
  filter(Emissions %in% c("Agriculture", "Non-agriculture"))

#transform wide to long data
methane2 <- methane1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = !Emissions, names_to = "year", values_to = "million tonnes carbon dioxide equivalent") |> 
  filter(year>"2001") |>
  mutate(Emissions=case_when(Emissions=="Agriculture" ~ "Agriculture", Emissions=="Non-agriculture" ~ "Non-agriculture"))
  
methane2$Emissions <- as.factor(methane2$Emissions)

af_categorical_colours <- afcolours::af_colours("categorical", n = 2)
names(af_categorical_colours)=levels(methane2$Emissions)

  #produce chart
methane_chart <- methane2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `million tonnes carbon dioxide equivalent`, group = Emissions, colour = Emissions), lwd = 1) +
  #scale_y_continuous(limits = c(0,110)) +
  scale_y_continuous(breaks = seq(0, 90, 10),limits = c(0, 90)) +
  scale_x_discrete(breaks = c(2002, 2007, 2012, 2017, 2022)) +
  scale_colour_manual(values = af_colours("categorical", n = 2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
  labs(x = NULL,
       y = "MtCO2e")
  
  methane_chart

save_graphic(methane_chart, "2.2.8c", "methane emissions")
save_csv(methane2, "2.2.8c", "methane emissions")

  #save graphic
#ggsave(filename = "2_2_8c_methane_emissions.svg",
#       methane_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)