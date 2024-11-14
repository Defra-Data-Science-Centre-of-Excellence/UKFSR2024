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

setwd("~/UKFSR/theme 2/files")

source("load-font.R")

SBI <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_2/2_1_2d_Soya_Bean_imports.csv")
#read in data
#SBI <- read_csv("2_1_2_Soya_Bean_imports.csv")

#filter data
SBI1<-SBI %>%
  group_by(`Soya Bean imports, thousand tonnes`) %>%
  filter(`Soya Bean imports, thousand tonnes` %in% c("EU", "Non EU")) %>%
  mutate(`Soya Bean imports, thousand tonnes`=factor(`Soya Bean imports, thousand tonnes`, levels=c("EU", "Non EU"), labels=c("EU", "Non EU")))

#croppable_area1$"Land use" <- as.factor(croppable_area1$"Land use")

#transform wide to long data
SBI2 <- SBI1 |> 
  #pivot_longer(cols = "2003":"2023", names_to = "year", values_to = "m.hectares") |> 
  pivot_longer(cols = !c("Soya Bean imports, thousand tonnes"), names_to = "year", values_to = "thousand tonnes") |> 
  mutate(year=as.numeric(year)) |>
  filter(year>2002)
  #mutate(year = factor(year, 
                       #levels = c("2003":"2023"),
                       #labels = c("2003":"2023")))# |>
  
SBI2$"Soya Bean imports, thousand tonnes" <- as.factor(SBI2$"Soya Bean imports, thousand tonnes")

af_categorical_colours <- afcolours::af_colours("categorical", n = 2)
names(af_categorical_colours)=levels(SBI2$`Soya Bean imports, thousand tonnes`)

  #filter(year>"2013") |>
soya_bean_import_chart <- SBI2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `thousand tonnes`, group = `Soya Bean imports, thousand tonnes`, colour = `Soya Bean imports, thousand tonnes`), lwd = 1) +
  #scale_y_continuous(limits = c(0,1000)) +
  scale_y_continuous(breaks = seq(0, 1000, 100),limits = c(0, 1000)) +
  #scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  scale_x_continuous(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Thousand tonnes")
  
  soya_bean_import_chart

save_graphic(soya_bean_import_chart, "2.1.2d", "soya bean imports")
save_csv(SBI, "2.1.2d", "soya bean imports")

#ggsave(filename = "2_1_2d_soya_bean_imports.svg",
       #soya_bean_import_chart,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)