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

source("load-font.R")

hort <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/input_data/t2_1_4/2_1_4a_c_d_fresh_fruit_and_vegetables.csv")

#hort <- read_csv("2_1_4a_fresh_fruit_and_vegetables.csv")

#filter data
hort1<-hort %>%
  group_by(`Supply and use 2`) %>%
  filter(`Supply and use 2` %in% c("Production as % of total new supply for use in the UK FF", "Production as % of total new supply for use in the UK FV")) %>%
  mutate(`Supply and use 2`=case_when(`Supply and use 2`=="Production as % of total new supply for use in the UK FF" ~ "Fresh fruit", `Supply and use 2`=="Production as % of total new supply for use in the UK FV" ~ "Fresh vegetables"))
  #mutate(usage=factor(`Supply and use 2`, levels=c("Fresh fruit 'production to supply' ratio", "Fresh vegetable 'production to supply' ratio")))

hort1$"Supply and use 2" <- as.factor(hort1$"Supply and use 2")

#transform wide to long data
hort2 <- hort1 |> 
  #pivot_longer(cols = "2013":"2023", names_to = "year", values_to = "percentage") |> 
  pivot_longer(cols = 4:42, names_to = "year", values_to = "percentage") |> 
  select(-Type, -`Supply and use 1`) |>
  mutate(`percentage`= as.numeric(`percentage`)) |>
  filter(year>"2002") |>
  mutate(year = factor(year, 
                       levels = c("2003":"2023"),
                       labels = c("2003":"2023")))# |>
  
hort2$`Supply and use 2` <- as.factor(hort2$`Supply and use 2`)

af_categorical_colours <- afcolours::af_colours("categorical", n = 2)
names(af_categorical_colours)=levels(hort2$`Supply and use 2`)

  #filter(year>"2013") |>
hort_chart_P2S <- hort2 |> 
  ggplot() +
  geom_line(aes(x = year, y = percentage/100, group = `Supply and use 2`, colour = `Supply and use 2`), lwd = 1) +
  #scale_y_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c(2003, 2008, 2013, 2018, 2023)) +
  scale_colour_manual(values = af_colours("categorical", n = 2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(x = NULL,
       y = NULL)
  
  hort_chart_P2S

save_graphic(hort_chart_P2S, "2.1.4a", "hort production to supply")
save_csv(hort2, "2.1.4a", "hort production to supply")

#ggsave(filename = "2_1_4_hort_P2S.svg",
       #hort_chart_P2S,
       #width = 960,
       #height = 640,
       #units = "px",
       #dpi = 72)