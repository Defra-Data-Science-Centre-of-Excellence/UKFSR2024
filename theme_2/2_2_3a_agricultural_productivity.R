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
library(stringr)

source(here::here("utils", "load-font.R"))

#setwd("~/UKFSR/theme 2/files")

#source("load-font.R")

#TFP <- read_csv("2_2_3_agricultural_productivity.csv")

TFP <- aws.s3::s3read_using(FUN = read_csv,
                                       bucket = ukfsr::s3_bucket(),
                                       object = "theme_2/input_data/t2_2_3/2_2_3a_agricultural_productivity.csv")


#filter data
#hort1<-hort %>%
  #group_by(`Supply and use 2`) %>%
  #filter(`Supply and use 2` %in% c("Production as % of total new supply for use in the UK FF", "Production as % of total new supply for use in the UK FV")) %>%
  #mutate(`Supply and use 2`=case_when(`Supply and use 2`=="Production as % of total new supply for use in the UK FF" ~ "Fresh fruit", `Supply and use 2`=="Production as % of total new supply for use in the UK FV" ~ "Fresh vegetables"))
  #mutate(usage=factor(`Supply and use 2`, levels=c("Fresh fruit 'production to supply' ratio", "Fresh vegetable 'production to supply' ratio")))

TFP$"Type" <- as.factor(TFP$"Type")

#transform wide to long data
TFP1 <- TFP |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = 2:52, names_to = "year", values_to = "index") |> 
  mutate(`index`= as.numeric(`index`)) |>
  filter(year>"1972") |>
  mutate(year = factor(year, 
                       levels = c("1973":"2023"),
                       labels = c("1973":"2023")))# |>
  
TFP1$`Type` <- as.factor(TFP1$`Type`)

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(TFP1$Type)

  #filter(year>"2013") |>
productivity_chart <- TFP1 |> 
  ggplot() +
  geom_line(aes(x = year, y = index, group = Type, colour = Type), lwd = 1) +
  #scale_y_continuous(limits = c(0,100)) +
  scale_y_continuous(breaks = seq(0, 175, 25),limits = c(0, 175), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(1973, 1983, 1993, 2003, 2013, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 3)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 30,  # Right margin
                             b = 0,  # Bottom margin
                             l = 10)) + # Left margin
  labs(x = NULL,
       y = "Index")
  
productivity_chart

save_graphic(productivity_chart, "2.2.3a", "productivity")
save_csv(TFP1, "2.2.3a", "productivity")

#ggsave(filename = "2_2_3a_productivity_chart.svg",
#       productivity_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)