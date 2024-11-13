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

#source("load-font.R")

agri_scheme_area <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_9/2_2_9a_agri_scheme_area.csv")

#agri_scheme_area <- read_csv("2_2_9a_agri_scheme_area.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
#croppable_area1<-croppable_area %>%
  #group_by(`Land use`) %>%
  #filter(`Land use` %in% c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops")) %>%
  #mutate(`Land use`=factor(`Land use`, levels=c("Total permanent grassland", "Total croppable area", "Total arable crops", "Potatoes", "Oilseeds", "Horticultural crops"), labels=c("Permanent grassland", "Croppable area", "Arable crops", "Potatoes", "Oilseeds", "Horticultural crops")))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

#transform wide to long data
agri_scheme_area1 <- agri_scheme_area |> 
  filter(`Scheme (Thousand hectares)`!="Total") |>
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "m.hectares") |> 
  pivot_longer(cols = !c("Scheme (Thousand hectares)"), names_to = "year", values_to = "Thousand hectares") |> 
  mutate(year=as.numeric(year)) |>
  filter(year>2020)
  #mutate(year = factor(year, 
                       #levels = c("2003":"2023"),
                       #labels = c("2003":"2023")))# |>
  
agri_scheme_area1$`Scheme (Thousand hectares)` <- factor(agri_scheme_area1$`Scheme (Thousand hectares)`, levels = c("Northern Ireland", "Wales", "Scotland", "England"))

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(agri_scheme_area1$`Scheme (Thousand hectares)`)

  #filter(year>"2013") |>

agri_scheme_chart <-
ggplot(agri_scheme_area1, aes(fill = `Scheme (Thousand hectares)`, x = year, y = `Thousand hectares`)) +
  geom_col(ggplot2::aes(fill = `Scheme (Thousand hectares)`, x = year, y = `Thousand hectares`)) +
  scale_x_continuous(breaks = c(2023, 2022, 2021)) +
  scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  labs(x = NULL, y = "Million hectares", title = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_colours("categorical", n = 4))
  #coord_flip()


#surface_water_chart <- surface_water1 |> 
  #ggplot() +
  #geom_line(aes(x = year, y = percentage, group = Percentage, colour = Percentage), lwd = 1) +
  #scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100)) +
  #scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  #scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  #scale_colour_manual(values = af_colours("categorical", n = 5)) +
  #theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  #labs(x = NULL, y = "percentage")
  
agri_scheme_chart

save_graphic(agri_scheme_chart, "2.2.9a", "agri scheme area")
save_csv(agri_scheme_area1, "2.2.9a", "agri scheme area")

#ggsave(filename = "2_2_9a_agri_scheme_area.svg",
#       agri_scheme_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)