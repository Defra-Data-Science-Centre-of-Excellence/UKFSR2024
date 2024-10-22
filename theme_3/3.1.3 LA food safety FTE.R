library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)

source(here::here("utils", "load-font.R"))

la_fte <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/la_allocated_food_safety_fte.csv")

cht <-
la_fte |> 
  ggplot() +
  geom_line(aes(x = fye, y = value, group = category, colour = category)) +
  scale_x_discrete(labels = c("2010/11", "", "2012/13", "", "2014/15", "", "2016/17", "", "2018/19", "", "2020/21", "", "2022/23", "")) +
  scale_y_continuous(limits = c(0,2000)) +
  scale_colour_manual(values = af_colours("duo")) +
  labs(x = NULL, y = "FTE posts") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(cht, "3.1.3c", "la allocated food safety fte")
save_csv(la_fte, "3.1.3c", "la allocated food safety fte")
