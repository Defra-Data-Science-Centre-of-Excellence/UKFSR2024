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

source(here::here("utils", "load-font.R"))

#setwd("~/UKFSR/theme 2/files")

#source("load-font.R")

all_species <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_5/2_2_5a_all_species.csv")

#import data to environment
#all_species <- read_csv("2_2_5_all_species.csv")

all_species_ci_lower<-all_species%>%
  select(Year,`Lower ci option 1`,`Lower ci option 2`)%>%
  pivot_longer(cols=2:3,values_to="lower_ci",names_to="ci")%>%
  mutate(option=if_else(ci%in%c("Lower ci option 1"),"Index option 1","Index option 2"))

all_species_ci_upper<-all_species%>%
  select(Year,`Upper ci option 1`,`Upper ci option 2`)%>%
  pivot_longer(cols=2:3,values_to="upper_ci",names_to="ci")%>%
  mutate(option=if_else(ci%in%c("Upper ci option 1"),"Index option 1","Index option 2"))

all_species_ci<-all_species_ci_lower%>%
  left_join(all_species_ci_upper,by=c("Year"="Year","option"="option"))

all_species_index<-all_species%>%select(Year,`Index option 1`,`Index option 2`)%>%pivot_longer(cols=2:3,values_to="value",names_to="option")

  #ggplot section
all_species_chart <- all_species |> 
  ggplot() +
  geom_line(data=`all_species_index`, aes(x = Year, y = value, colour = option), lwd = 1) +
  geom_ribbon(data=`all_species_ci`, aes(x = Year, ymin = `lower_ci`, ymax = `upper_ci`, fill = option),alpha=0.2) +
  scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100)) +
  scale_colour_manual(values = af_colours("duo")) +
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Index (1970 = 100)")
  
  all_species_chart

save_graphic(all_species_chart, "2.2.5a", "all species abundance")
save_csv(all_species, "2.2.5a", "all species abundance")

#ggsave(filename = "2_2_5a_all_species.svg",
#       all_species_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)
