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

source(here("utils", "load-font.R"))

t5_1_1a <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_1_7/output/csv/5_1_7a_perc_food_bus_rating_3_Eng_Wales_NI.csv")

t5_1_7a$quarter <- factor(t5_1_7a$quarter, levels = c("Q1 2019/20","Q2 2019/20","Q3 2019/20","Q4 2019/20","Q1 2020/21","Q2 2020/21",
                                                      "Q3 2020/21","Q4 2020/21","Q1 2021/22","Q2 2021/22","Q3 2021/22","Q4 2021/22",
                                                      "Q1 2022/23","Q2 2022/23","Q3 2022/23","Q4 2022/23","Q1 2023/24","Q2 2023/24",
                                                      "Q3 2023/24","Q4 2023/24"))


af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_1_7a$quarter_wrap = str_wrap(t5_1_7a$quarter, width = 4)

t5_1_7a$quarter_wrap <- as.factor(t5_1_7a$quarter_wrap)

level_order <- c("Q1\n2019/20","Q2\n2019/20","Q3\n2019/20","Q4\n2019/20","Q1\n2020/21","Q2\n2020/21",
                 "Q3\n2020/21","Q4\n2020/21","Q1\n2021/22","Q2\n2021/22","Q3\n2021/22","Q4\n2021/22",
                 "Q1\n2022/23","Q2\n2022/23","Q3\n2022/23","Q4\n2022/23","Q1\n2023/24","Q2\n2023/24",
                 "Q3\n2023/24","Q4\n2023/24")

t5_1_7a_plot <- ggplot(t5_1_7a, aes(x=factor(quarter_wrap), y=total, group =1)) +
  theme_classic() +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  geom_line(linewidth=1, colour = af_colours_1) +
  labs(y = "Percentage of respondents (%)") +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10)) +
  theme(axis.text.x = element_text(size=10, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm"))

t5_1_7a_plot

save_graphic(t5_1_7a_plot, "5.1.7a", "perc food bus rating 3 Eng Wales NI")

