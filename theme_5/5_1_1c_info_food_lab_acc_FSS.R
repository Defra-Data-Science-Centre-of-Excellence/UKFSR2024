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

t5_1_1c <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_1_1/output/csv/5_1_1c_info_food_lab_acc_FSS.csv")

t5_1_1c <- fread("5_1_1c_info_food_lab_acc_FSS.csv")

t5_1_1c$Wave <- factor(t5_1_1c$Wave, levels = c("Wave 11 (12/2020)","Wave 13 (12/2021)","Wave 15 (12/2022)"))

t5_1_1c_long <- t5_1_1c %>% 
  group_by(Wave) %>%
  pivot_longer(cols=c("Agree","I neither agree nor disagree","Disagree","Don't know"),
               names_to="Response",
               values_to="Value")

t5_1_1c_long$Response <- factor(t5_1_1c_long$Response, levels = c("Agree","I neither agree nor disagree","Disagree","Don't know"))

level_order <- c("Agree","I neither agree nor disagree","Disagree","Don't know")

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(t5_1_1c_long$Response)

t5_1_1c_long$Wave_wrap = str_wrap(t5_1_1c_long$Wave, width = 14)

t5_1_1c_plot <- ggplot(t5_1_1c_long,aes(x=Wave_wrap, y=Value, label = round(Value,0), group=Response)) +
  geom_bar(stat="identity", aes(fill = Response)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  scale_y_continuous(limits = c(0,100.01),breaks = seq(0,100.01, 10))+
  labs(y = "Percentage of respondents (%)") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.x = element_blank()) +
  geom_text(stat = "stratum", aes(stratum = Response), color="white", fontface = "bold", size=6) 

t5_1_1c_plot

save_graphic(t5_1_1c_plot, "5.1.1c", "info food lab acc fss")

save_csv(t5_1_1c, "5.1.1c", "info food lab acc fss")

