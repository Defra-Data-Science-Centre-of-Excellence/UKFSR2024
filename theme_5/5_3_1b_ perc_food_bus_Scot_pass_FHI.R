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

t5_3_1b <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_3_1/output/csv/5_3_1b_perc_food_bus_Scot_pass_FHI.csv")

t5_3_1b$quarter <- factor(t5_3_1b$quarter, levels = c("Q1 2019/20","Q2 2019/20","Q3 2019/20","Q4 2019/20","Q1 2020/21","Q2 2020/21",
                                                      "Q3 2020/21","Q4 2020/21","Q1 2021/22","Q2 2021/22","Q3 2021/22","Q4 2021/22",
                                                      "Q1 2022/23","Q2 2022/23","Q3 2022/23","Q4 2022/23","Q1 2023/24","Q2 2023/24",
                                                      "Q3 2023/24","Q4 2023/24"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_3_1b$quarter_wrap = str_wrap(t5_3_1b$quarter, width = 4)

t5_3_1b$quarter_wrap <- as.factor(t5_3_1b$quarter_wrap)

level_order <- c("Q1\n2019/20","Q2\n2019/20","Q3\n2019/20","Q4\n2019/20","Q1\n2020/21","Q2\n2020/21",
                 "Q3\n2020/21","Q4\n2020/21","Q1\n2021/22","Q2\n2021/22","Q3\n2021/22","Q4\n2021/22",
                 "Q1\n2022/23","Q2\n2022/23","Q3\n2022/23","Q4\n2022/23","Q1\n2023/24","Q2\n2023/24",
                 "Q3\n2023/24","Q4\n2023/24")

t5_3_1b_plot <- ggplot(t5_3_1b, aes(x=factor(quarter_wrap), y=total, group =1)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  geom_line(linewidth=1, colour = af_colours_1) +
  labs(y = "Percentage of respondents (%)") +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10)) +
  theme(axis.text.x = element_text(size=10, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm"))

t5_3_1b_plot

save_graphic(t5_3_1b_plot, "5.3.1b", "perc food bus Scot pass FHI")

save_csv(t5_3_1b, "5.3.1b", "perc food bus Scot pass FHI")
