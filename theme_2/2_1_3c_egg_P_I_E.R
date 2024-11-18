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

source("load-font.R")

eggs <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_3/2_1_3c_eggs.csv")

#eggs <- read_csv("2_1_3_eggs.csv")
#beef <- fread("2_1_3_beef.csv")

#beef <- beef %>%  row_to_names(row_number = 1)

#beef1 <- beef |> 
  #filter(Production=="Production" & Production=="Imports" & Production=="Exports")

#filter data
eggs1<-eggs %>%
  group_by(Population) %>%
  filter(Population %in% c("UK production of eggs for human consumption (million doz)", "Imports", "Exports")) %>%
  mutate(Population=factor(Population, levels=c("UK production of eggs for human consumption (million doz)", "Imports", "Exports"), labels=c("UK production", "Imports", "Exports")))

#transform wide to long data
eggs2 <- eggs1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "m. dozen") |> 
  pivot_longer(cols = !Population, names_to = "year", values_to = "m. dozen") |> 
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
eggs2$Population <- as.factor(eggs2$Population)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(eggs2$Population)

  #filter(year>"2013") |>
egg_chart_prod <- eggs2 |> 
  ggplot() +
  geom_line(aes(x = year, y = `m. dozen`, group = Population, colour = Population), lwd = 1) +
  #scale_y_continuous(limits = c(0,1000)) +
  scale_y_continuous(breaks = seq(0, 1100, 100),limits = c(0, 1100), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Million dozen")
  
  egg_chart_prod

save_graphic(egg_chart_prod, "2.1.3c", "Egg sector")
save_csv(eggs1, "2.1.3c", "Egg sector")

#ggsave(filename = "2_1_3c_domestic_egg_supply.svg",
       #egg_chart_prod,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)