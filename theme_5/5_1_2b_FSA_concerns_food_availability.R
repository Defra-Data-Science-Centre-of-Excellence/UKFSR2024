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
library(scales)

source(here("utils", "load-font.R"))

t5_1_2b <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_1_2/output/csv/5_1_2b_fsa_respond_concern_about_food_avail.csv",
                                col_types = colspec)

level_order <- c("Highly concerned","Somewhat concerned","Not very concerned","Not at all concerned","Don't know")

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_1_2b$concern_wrap <- str_wrap(t5_1_2b$`Consumer concern about the availability of food`, width = 200)

t5_1_2b_plot <- ggplot(t5_1_2b, aes(x= factor(concern_wrap, level = level_order), y=`Percentage of respondents`)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values=af_colours_1) +
  scale_x_discrete(labels = label_wrap(10)) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  labs(y = "Percentage of respondents (%)") +
  theme(axis.title.x=element_blank()) +
  geom_text(aes(label = round(`Percentage of respondents`,0)), vjust= 1.5, hjust = 0.5, size = 8, fontface = "bold", colour = "white") 

t5_1_2b_plot

save_graphic(t5_1_2b_plot, "5.1.2b", "fsa respond concern about food avail")

save_csv(t5_1_2b, "5.1.2b", "fsa respond concern about food avail")

