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

organic <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_9/2_2_9b_organic_land_area.csv")

#organic <- read_csv("2_2_3_organic_land_area.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
organic1<-organic %>%
  group_by(type_region) %>%
  filter(type_region %in% c("In-conversion UK", "Fully organic UK", "Total UK"))

organic1$type_region <- as.factor(organic1$type_region)

#transform wide to long data
organic2 <- organic1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "th.hectares") |> 
  pivot_longer(cols = !c("type_region","Type","Region"), names_to = "year", values_to = "th.hectares") |> 
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
organic2$type_region <- as.factor(organic2$type_region)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(organic2$Production)

  #filter(year>"2013") |>
organic_chart <- organic2 |> 
  ggplot() +
  geom_line(aes(x = year, y = th.hectares, group = type_region, colour = type_region), lwd = 1) +
  #scale_y_continuous(limits = c(0,600)) +
  scale_y_continuous(breaks = seq(0, 800, 100),limits = c(0, 800)) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Thousand hectares")
  
  organic_chart

save_graphic(organic_chart, "2.2.9b", "organic area")
save_csv(organic2, "2.2.9b", "organic area")

#ggsave(filename = "2_2_9b_organic_area.svg",
#       organic_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)