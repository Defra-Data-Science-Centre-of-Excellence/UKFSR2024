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

source(here("utils", "load-font.R"))

t5_1_2c <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_1_2/output/csv/5_1_2c_FSA_respond_concern_about_food_avail.csv",
                                col_types = colspec)

level_order <- c("Highly concerned","Somewhat concerned","Not very concerned","Not at all concerned","Don't know")

t5_1_2c_plot <- ggplot(t5_1_2c, aes(x= factor(`Consumer concern about the availability of food`, level = level_order), y=`Percentage of respondents`)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  labs(y = "Percentage of respondents (%)") +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label = round(`Percentage of respondents`,0)), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white") 

t5_1_2c_plot

save_graphic(t5_1_2c_plot, "5.1.2c", "fsa respond concern about food avail")

save_csv(t5_1_2c, "5.1.2c", "fsa respond concern about food avail")

