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
library(ggrepel)

source(here("utils", "load-font.R"))

t5_3_1d <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_3_1/output/csv/5_3_1d_no_food_bus_iss_food_hyg_rating_quarter_scot.csv")


t5_3_1d$quarter <- factor(t5_3_1d$quarter, levels = c("Q1 2019/20","Q2 2019/20","Q3 2019/20","Q4 2019/20","Q1 2020/21","Q2 2020/21",
                                                      "Q3 2020/21","Q4 2020/21","Q1 2021/22","Q2 2021/22","Q3 2021/22","Q4 2021/22",
                                                      "Q1 2022/23","Q2 2022/23","Q3 2022/23","Q4 2022/23","Q1 2023/24","Q2 2023/24",
                                                      "Q3 2023/24","Q4 2023/24"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_3_1d$quarter_wrap = str_wrap(t5_3_1d$quarter, width = 4)

t5_3_1d$quarter_wrap <- as.factor(t5_3_1d$quarter_wrap)

level_order <- c("Q1\n2019/20","Q2\n2019/20","Q3\n2019/20","Q4\n2019/20","Q1\n2020/21","Q2\n2020/21",
                 "Q3\n2020/21","Q4\n2020/21","Q1\n2021/22","Q2\n2021/22","Q3\n2021/22","Q4\n2021/22",
                 "Q1\n2022/23","Q2\n2022/23","Q3\n2022/23","Q4\n2022/23","Q1\n2023/24","Q2\n2023/24",
                 "Q3\n2023/24","Q4\n2023/24")

# https://stackoverflow.com/questions/70441422/control-discrete-tick-labels-in-ggplot2-scale-x-discrete
everyother <- function(x) x[seq_along(x) %% 2 == 0]

t5_3_1d_plot <- ggplot(t5_3_1d, aes(x=factor(quarter_wrap), y= FHR, group =1)) +
  theme_classic() +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  geom_line(linewidth=1, colour = af_colours_1) +
  labs(y = "Number of food businesses issued a food hygiene rating") +
  scale_x_discrete(breaks = everyother, limits = level_order) +
  scale_y_continuous(limits = c(0,6000), breaks = seq(0,6000,1000), labels = comma) +
  geom_text_repel(aes(label = comma(FHR)), min.segment.length = 0, seed = 42, box.padding = 0.7, size = 7, vjust = -1, show.legend  = F) +
  theme(axis.text.x = element_text(size=18)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(plot.margin = margin(t = 30,  # Top margin
                             r = 40,  # Right margin
                             b = 10,  # Bottom margin
                             l = 5))  # Left margin

t5_3_1d_plot

save_graphic(t5_3_1d_plot, "5.3.1d", "no food bus iss food hyg rating quarter scot")

save_csv(t5_3_1d, "5.3.1d", "no food bus iss food hyg rating quarter scot")


