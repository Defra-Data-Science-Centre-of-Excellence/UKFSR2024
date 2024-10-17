library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(patchwork)
library(grid)
library(gridtext)
library(gtable)
library(ggtext)
library(data.table)
library(ggalluvial)
library(scales)

t5_3_1e <- fread("5_3_1e_no_food_bus_iss_food_hyg_rating_quarter_Eng_Wales_NI.csv")

source(here("utils", "load-font.R"))

t5_3_1e <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_3_1/output/csv/5_3_1e_no_food_bus_iss_food_hyg_rating_quarter_Eng_Wales_NI.csv")

t5_3_1e$month <- factor(t5_3_1e$month, levels = c("April","May","June","July","August","September","October","November","December","January","February","March"))

t5_3_1e_long <- t5_3_1e %>% 
  group_by(month) %>%
  pivot_longer(cols=c("2019/20","2020/21","2021/22","2022/23","2023/24"),
               names_to="year",
               values_to="value")

t5_3_1e_long$year <- factor(t5_3_1e_long$year, levels = c("2019/20","2020/21","2021/22","2022/23","2023/24"))

level_order <- c("2019/20","2020/21","2021/22","2022/23","2023/24")

af_categorical_colours <- afcolours::af_colours("categorical", n = 5)
names(af_categorical_colours)=levels(t5_3_1e_long$year)

# # https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# # ensure question axis matches original vector
# # https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
# # Turn question column into a character vector
# # Then turn it back into a factor with the levels in the correct order

  t5_3_1e_plot <- ggplot(t5_3_1e_long,aes(x=month, y=value, group=year, colour=year)) +
  geom_line(linewidth=1) +
  scale_colour_manual(values = af_categorical_colours) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(limits = c(0,25000),breaks = seq(0,25000, 2500), labels = scales::comma_format()) +
  labs(y = "Food Hygiene Ratings Issued") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(size=20, face = "italic")) +
  theme(axis.title.x = element_blank())  +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    
t5_3_1e_plot

save_graphic(t5_3_1e_plot, "5.3.1e", "no food bus iss food hyg rating quarter Eng Wales NI")

save_csv(t5_3_1e, "5.3.1e", "no food bus iss food hyg rating quarter Eng Wales NI")

