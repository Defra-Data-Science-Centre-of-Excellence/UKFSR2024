library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(janitor)
library(scales)
library(purrr)
library(lubridate)
library(countrycode)
library(mm23)
library(ukfsr)
library(data.table)
library(zoo)
library(afcolours)
library(dplyr)

source(here("utils", "load-font.R"))

t3_1_3e <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_3/output/csv/3_1_3e_local_authority_unfilled_food_hygiene_posts.csv")

t3_1_3e$`Financial Year` <- factor(t3_1_3e$`Financial Year`, levels = c("2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"))

t3_1_3e_long <- t3_1_3e

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t3_1_3e_plot <- ggplot(t3_1_3e_long, aes(x=`Financial Year`, y=`Unfilled FH FTE`, group = 1)) +
  geom_line(linewidth=1) +
  geom_text(aes(label = scales::percent(`Unfilled FH FTE`, accuracy = 1)), size = 7, vjust = -1, show.legend  = F) +
  scale_colour_manual(values = af_colours_1) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Unfilled Food hygiene posts (FTE)") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=20, face = "italic")) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

t3_1_3e_plot

save_graphic(t3_1_3e_plot, "3.1.3e", "local authority unfilled food hygiene posts")

save_csv(t3_1_3e, "3.1.3e", "local authority unfilled food hygiene posts")
