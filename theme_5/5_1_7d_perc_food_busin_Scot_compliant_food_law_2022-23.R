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

t5_1_1d <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_1_7/output/csv/5_1_7d_perc_food_busin_Scot_compliant_food_law_2022-23.csv")

t5_1_7d$year <- factor(t5_1_7d$year, levels = c("2022","2023"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

  t5_1_7d_plot <- ggplot(t5_1_7d, aes(x= year, y= perc)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  labs(y = "Percentage of businesses (%)") +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label=sprintf("%.1f", perc)), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white")

  t5_1_7d_plot

  save_graphic(t5_1_7d_plot, "5.1.7d", "perc food busin Scot compliant food law 2022-23")

