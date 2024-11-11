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

t4_3_2c <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_4/t4_3_2/output/csv/4_3_2c_perc_income_eatwell.csv")

# t4_3_2c$`Income Quintile (most to least deprived)` <- factor(t4_3_2c$`Income Quintile (most to least deprived)`, levels = c("1","2","3","4","5"))

level_order <- c("1","2","3","4","5")

af_colours_1 <- c(
  "#12436D" # Dark blue
)

# af_colours_5  <- c("#12436D","#28A197","#801650","#F46A25","#3D3D3D")

t4_3_2c_plot <- ggplot(t4_3_2c, aes(x= factor(`Income Quintile (most to least deprived)`, level = level_order), y=`Percentage of disposable income (%)`)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,50), breaks=seq(0,50,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(x = "Income Quintile (most to least deprived)") +
  geom_text(aes(label = sprintf("%.1f",`Percentage of disposable income (%)`)), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white") 

t4_3_2c_plot

save_graphic(t4_3_2c_plot, "4.3.2c", "perc income eatwell")

save_csv(t4_3_2c, "4.3.2c", "perc income eatwell")
