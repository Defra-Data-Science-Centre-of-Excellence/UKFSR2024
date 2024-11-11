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

t3_1_3d <- aws.s3::s3read_using(FUN = read_csv,
                              bucket = ukfsr::s3_bucket(),
                              object = "theme_3/t3_1_3/output/csv/3_1_3d_no_alloc_food_law_officers_FTE_Scot_2016_2024.csv")

t3_1_3d$Year <- factor(t3_1_3d$Year, levels = c("2016","2017","2018","2019","2021","2022","2023","2024"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

  t3_1_3d_plot <- ggplot(t3_1_3d, aes(x= Year, y= `Food Law Officers FTE`)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,275), breaks=seq(0,275,25)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  labs(y = "Food Law Officers FTE") +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label=`Food Law Officers FTE`), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white")

  t3_1_3d_plot


save_graphic(t3_1_3d_plot, "3.1.3d", "no alloc food law officers FTE Scot 2016 2024")

save_csv(t3_1_3d, "3.1.3d", "no alloc food law officers FTE Scot 2016 2024")

