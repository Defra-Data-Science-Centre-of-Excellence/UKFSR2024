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

meat <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_3/2_1_3b_meat.csv")

#meat <- read_csv("2_1_3_meat.csv")
#meat <- fread("2_1_3_meat.csv")

#meat <- meat %>%  row_to_names(row_number = 1)

#meat1 <- meat |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
meatprod1<-meat %>%
  group_by(Population) %>%
  filter(Population %in% c("Production beef", "Production lamb", "Production pigs", "Production poultry")) %>%
  mutate(Population=case_when(Population=="Production beef" ~ "Beef", Population=="Production lamb" ~ "Lamb", Population=="Production pigs" ~ "Pork", Population=="Production poultry" ~ "Poultry")) %>%
  mutate(Population=factor(Population, levels=c("Beef", "Lamb", "Pork", "Poultry")))


#transform wide to long data
meatprod2 <- meatprod1 |> 
  #pivot_longer(cols = "1990":"2023", names_to = "year", values_to = "th. tonnes") |> 
  pivot_longer(cols = !Population, names_to = "year", values_to = "th. tonnes") |> 
  mutate(year=as.numeric(year)) |>
  mutate("m. tonnes"=`th. tonnes`/(1000)) |>
  filter(year>2002)

#meatprod2$Population <- as.factor(meatprod2$Population)

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(meatprod2$Population)

#make chart
meat_chart_prod <-
  ggplot(meatprod2, aes(group = Population, x = year, y = `m. tonnes`)) +
  geom_line(ggplot2::aes(group = Population, colour = Population, x = year, y = `m. tonnes`), lwd = 1) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  labs(x = NULL, y = "Million tonnes", title = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_colour_manual(values = af_colours("categorical", n = 4))
  
  meat_chart_prod

save_graphic(meat_chart_prod, "2.1.3b", "Meat Production")
save_csv(meat, "2.1.3b", "Meat production")

#ggsave(filename = "2_1_3b_meat_prod.svg",
       #meat_chart_prod,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)