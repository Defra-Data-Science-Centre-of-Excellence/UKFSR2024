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

overall_GHG <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_8/2_2_8a_overall_GHG.csv")%>%
  select(-`2023`,-`2024`)

#overall_GHG <- read_csv("2_2_8_overall_GHG.csv")
#organic <- fread("2_2_3_orgaic_land_area.csv")

#organic <- organic %>%  row_to_names(row_number = 1)

#organic1 <- organic |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
overall_GHG1<-overall_GHG %>%
  group_by(`category (MtCO2e)`) %>%
  filter(`category (MtCO2e)` %in% c("Electricity supply total", "Fuel supply total", "Domestic transport total", "Agriculture total", "Waste total", "LULUCF total")) %>%
  mutate(`category (MtCO2e)`=factor(`category (MtCO2e)`, levels=c("Electricity supply total", "Fuel supply total", "Domestic transport total", "Agriculture total", "Waste total", "LULUCF total"), labels=c("Electricity", "Fuel", "Transport", "Agriculture", "Waste", "Land use")))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

#transform wide to long data
overall_GHG2 <- overall_GHG1 |> 
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "m.hectares") |> 
  pivot_longer(cols = !c(`category (MtCO2e)`), names_to = "year", values_to = "(MtCO2e)") |> 
  mutate(year=as.numeric(year)) |>
  filter(year>2001)
  #mutate(year = factor(year, 
                       #levels = c("2003":"2023"),
                       #labels = c("2003":"2023")))  # |>
  
#surface_water1$Quality <- factor(surface_water1$Quality, levels = c("Bad", "Poor", "Moderate", "Good", "High"))

af_categorical_colours <- afcolours::af_colours("categorical", n = 6)
names(af_categorical_colours)=levels(overall_GHG2$`category (MtCO2e)`)

  #filter(year>"2013") |>

overall_GHG_chart <-
ggplot(overall_GHG2, aes(fill = `category (MtCO2e)`, x = year, y = `(MtCO2e)`)) +
  geom_col(ggplot2::aes(fill = `category (MtCO2e)`, x = year, y = `(MtCO2e)`)) +
  scale_x_continuous(breaks = c(2002, 2007, 2012, 2017, 2022)) +
  labs(x = NULL, y = "(MtCO2e)", title = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_colours("categorical", n = 6))
  #ggplot2::coord_flip()


#surface_water_chart <- surface_water1 |> 
  #ggplot() +
  #geom_line(aes(x = year, y = percentage, group = Percentage, colour = Percentage), lwd = 1) +
  #scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100)) +
  #scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  #scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  #scale_colour_manual(values = af_colours("categorical", n = 5)) +
  #theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  #labs(x = NULL, y = "percentage")
  
overall_GHG_chart

save_graphic(overall_GHG_chart, "2.2.8a", "overall GHG emissions")
save_csv(overall_GHG2, "2.2.8a", "overall GHG emissions")

#ggsave(filename = "2_2_8a_overall_GHG.svg",
#       overall_GHG_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)