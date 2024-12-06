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

priority_species <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_2_5/2_2_5b_priority_species.csv")

#import data to environment
#priority_species <- read_csv("2_2_5_priority_species.csv")

priority_species_ci_lower<-priority_species%>%
  select(Year,`Lower ci option 1`,`Lower ci option 2`)%>%
  pivot_longer(cols=2:3,values_to="lower_ci",names_to="ci")%>%
  mutate(option=if_else(ci%in%c("Lower ci option 1"),"Index option 1","Index option 2"))

priority_species_ci_upper<-priority_species%>%
  select(Year,`Upper ci option 1`,`Upper ci option 2`)%>%
  pivot_longer(cols=2:3,values_to="upper_ci",names_to="ci")%>%
  mutate(option=if_else(ci%in%c("Upper ci option 1"),"Index option 1","Index option 2"))

priority_species_ci<-priority_species_ci_lower%>%
  left_join(priority_species_ci_upper,by=c("Year"="Year","option"="option"))

priority_species_index<-priority_species%>%select(Year,`Index option 1`,`Index option 2`)%>%pivot_longer(cols=2:3,values_to="value",names_to="option")

  #ggplot section
priority_species_chart <- priority_species |> 
  ggplot() +
  geom_line(data=`priority_species_index`, aes(x = Year, y = value, colour = option), lwd = 1) +
  geom_ribbon(data=`priority_species_ci`, aes(x = Year, ymin = `lower_ci`, ymax = `upper_ci`, fill = option),alpha=0.2) +
  scale_y_continuous(breaks = seq(0, 100, 10),limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = af_colours("duo")) +
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = "Index (1970 = 100)")
  
  priority_species_chart

save_graphic(priority_species_chart, "2.2.5b", "priority species")
save_csv(priority_species, "2.2.5b", "priority species")

#ggsave(filename = "2_2_5b_priority_species.svg",
#       priority_species_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)
