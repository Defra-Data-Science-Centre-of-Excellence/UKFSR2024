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

t5_3_1h <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_3_1/output/csv/5_3_1h_perc_meat_establish_rated_good_hyg_Scot_2022-23.csv")

t5_3_1h$year <- factor(t5_3_1h$year, levels = c("2022","2023"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

# https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# ensure question axis matches original vector
# https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
# Turn question column into a character vector
# Then turn it back into a factor with the levels in the correct order

t5_3_1h_plot <- ggplot(t5_3_1h, aes(x= year, y= perc)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  labs(y = "Percentage of businesses (%)") +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label=sprintf("%.1f", perc)), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white")

  t5_3_1h_plot

save_graphic(t5_3_1h_plot, "5.3.1h", "perc meat establish rated good hyg Scot 2022-23")

save_csv(t5_3_1h, "5.3.1h", "perc meat establish rated good hyg Scot 2022-23")

