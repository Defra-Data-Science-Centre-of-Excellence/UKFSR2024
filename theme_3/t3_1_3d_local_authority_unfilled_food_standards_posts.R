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

t3_1_3d <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_3/output/csv/3_1_3d_local_authority_unfilled_food_standards_posts.csv")

t3_1_3d$`Financial Year` <- factor(t3_1_3d$`Financial Year`, levels = c("2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"))

t3_1_3d_long <- t3_1_3d

af_colours_1 <- c(
  "#12436D" # Dark blue
)

# wrapping of a single word
# https://stackoverflow.com/questions/51515890/stringrstr-wrap-does-not-wrap-exactly-every-n-characters
t3_1_3d_long$Year_wrap = str_replace_all(t3_1_3d_long$`Financial Year`, paste0("(.{5})"), "\\1\n")

t3_1_3d_plot <- ggplot(t3_1_3d_long, aes(x=`Financial Year`, y=`Unfilled FS FTE`, group = 1)) +
  geom_line(linewidth=1) +
  geom_text(aes(label = scales::percent(`Unfilled FS FTE`, accuracy = 1)), size = 7, vjust = -1, show.legend  = F) +
  scale_colour_manual(values = af_colours_1) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Unfilled Food Standards posts (FTE)") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=20, face = "italic")) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

t3_1_3d_plot

save_graphic(t3_1_3d_plot, "3.1.3d", "local authority unfilled food standards posts")

save_csv(t3_1_3d, "3.1.3d", "local authority unfilled food standards posts")

