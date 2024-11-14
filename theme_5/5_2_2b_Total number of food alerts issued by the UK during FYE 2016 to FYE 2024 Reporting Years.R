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

t5_2_2b <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_2_2/output/csv/5_2_2b_Total_number_of_food_alerts_issued_by_the_UK_during_FYE_2016_to_FYE_2024_Reporting_Years.csv")

t5_2_2b$Alert <- factor(t5_2_2b$Alert, levels = c("AA","PRIN","FAFA"))

t5_2_2b_long <- t5_2_2b %>% 
  group_by(Alert) %>%
  pivot_longer(cols=c("2015/16","2016/17","2017/18","2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"),
               names_to="Year",
               values_to="Value")

t5_2_2b_long$Year <- factor(t5_2_2b_long$Year, levels = c("2015/16","2016/17","2017/18","2018/19","2019/20","2020/21","2021/22",
                                                          "2022/23","2023/24"))

level_order <- c("2015/16","2016/17","2017/18","2018/19",
                 "2019/20","2020/21","2021/22","2022/23",
                 "2023/24")

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(t5_2_2b_long$Alert)
 
  t5_2_2b_plot <- ggplot(t5_2_2b_long,aes(x=Year, y=Value, label = round(Value,0), group=Alert)) +
  geom_bar(aes(fill = Alert), stat = "identity", position = "dodge") +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  labs(y = "Total number of food alerts") +
  theme(axis.text.x = element_text(size=22)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.x = element_blank()) +
  geom_text(aes(label=Value), position=position_dodge(width=0.9), fontface = "bold", size=8, vjust = -0.8) +
  coord_cartesian(clip = "off")
  
t5_2_2b_plot

save_graphic(t5_2_2b_plot, "5.2.2d", "total number of food alerts issued by the UK during FYE 2016 to FYE 2024 reporting years")

save_csv(t5_2_2b, "5.2.2d", "total number of food alerts issued by the UK during FYE 2016 to FYE 2024 reporting years")

