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

t5_3_1a <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_3_1/output/csv/5_3_1a_perc_distr_fhrs_Eng_Wales_NI.csv")

t5_3_1a$quarter <- factor(t5_3_1a$quarter, levels = c("Q1 2019/20","Q2 2019/20","Q3 2019/20","Q4 2019/20","Q1 2020/21","Q2 2020/21",
                                                      "Q3 2020/21","Q4 2020/21","Q1 2021/22","Q2 2021/22","Q3 2021/22","Q4 2021/22",
                                                      "Q1 2022/23","Q2 2022/23","Q3 2022/23","Q4 2022/23","Q1 2023/24","Q2 2023/24",
                                                      "Q3 2023/24","Q4 2023/24"))

t5_3_1a_long <- t5_3_1a %>% 
  pivot_longer(cols=c("5 - Very good","4 - Good","3 - Generally satisfactory","2 - Improvement necessary",
                      "1 - Major improvement necessary","0 - Urgent improvement necessary"),
               names_to="rating",
               values_to="perc")  %>% 
  group_by(rating)

t5_3_1a_long$rating <- factor(t5_3_1a_long$rating, levels = c("5 - Very good","4 - Good","3 - Generally satisfactory",
                                                              "2 - Improvement necessary","1 - Major improvement necessary",
                                                              "0 - Urgent improvement necessary"))

af_categorical_colours <- afcolours::af_colours("categorical", n = 6)
names(af_categorical_colours)=levels(t5_3_1a_long$rating)

t5_3_1a_long$rating <- factor(t5_3_1a_long$rating, levels = rev(levels(t5_3_1a_long$rating)))

t5_3_1a_long$quarter_wrap = str_wrap(t5_3_1a_long$quarter, width = 4)

t5_3_1a_long$quarter_wrap <- as.factor(t5_3_1a_long$quarter_wrap)

level_order <- c("Q1\n2019/20","Q2\n2019/20","Q3\n2019/20","Q4\n2019/20","Q1\n2020/21","Q2\n2020/21",
                 "Q3\n2020/21","Q4\n2020/21","Q1\n2021/22","Q2\n2021/22","Q3\n2021/22","Q4\n2021/22",
                 "Q1\n2022/23","Q2\n2022/23","Q3\n2022/23","Q4\n2022/23","Q1\n2023/24","Q2\n2023/24",
                 "Q3\n2023/24","Q4\n2023/24")

# https://stackoverflow.com/questions/70441422/control-discrete-tick-labels-in-ggplot2-scale-x-discrete
everyother <- function(x) x[seq_along(x) %% 2 == 0]

  t5_3_1a_plot <- ggplot(t5_3_1a_long,aes(x=quarter_wrap, y=perc, group=rating)) +
  geom_area(color = "white", aes(fill = rating)) +
  scale_x_discrete(breaks = everyother, limits = level_order) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  scale_y_continuous(expand = c(0,-1), limits = c(0,101),breaks = seq(0,101,10)) +
  labs(y = "Percentage of FHRS ratings (%)") +
  theme(axis.text.x = element_text(size=18)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=20, face = "italic", 
        margin = margin(r = 2, unit = 'cm'))) + 
  theme(legend.spacing.x = unit(1.0, "cm")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(plot.margin = margin(t = 10,  # Top margin
                             r = 40,  # Right margin
                             b = 10,  # Bottom margin
                             l = 5))  # Left margin

t5_3_1a_plot

save_graphic(t5_3_1a_plot, "5.3.1a", "perc distr fhrs Eng Wales NI")

save_csv(t5_3_1a, "5.3.1a", "perc distr fhrs Eng Wales NI")

