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

t5_1_1e <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_1_1/output/csv/5_1_1e_trust_FSS.csv")

t5_1_1e$Wave <- factor(t5_1_1e$Wave, levels = c("Wave 11 (12/2020)","Wave 12 (07/2021)","Wave 13 (12/2021)","Wave 14 (07/2022)","Wave 15 (12/2022)","Wave 16 (07/2023)","Wave 17 (12/2023)"))

t5_1_1e_long <- t5_1_1e %>% 
  group_by(Wave) %>%
  pivot_longer(cols=c("Trust","Neither trust nor distrust","Distrust","Don't know"),
               names_to="Response",
               values_to="Value")

t5_1_1e_long$Response <- factor(t5_1_1e_long$Response, levels = c("Trust","Neither trust nor distrust","Distrust","Don't know"))

level_order <- c("Trust","Neither trust nor distrust","Distrust","Don't know")

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(t5_1_1e_long$Response)

t5_1_1e_long$Wave_wrap = str_wrap(t5_1_1e_long$Wave, width = 14)


t5_1_1e_plot <- ggplot(t5_1_1e_long,aes(x=Wave_wrap, y=Value, label = round(Value,0), group=Response)) +
  geom_bar(stat="identity", aes(fill = Response)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  scale_y_continuous(limits = c(0,101),breaks = seq(0,100.01, 10))+
  labs(y = "Percentage of respondents (%)") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.x = element_blank()) +
  geom_text(stat = "stratum", aes(stratum = Response), color="white", fontface = "bold", size=6) 

t5_1_1e_plot

save_graphic(t5_1_1e_plot, "5.1.1e", "trust in fss")

save_csv(t5_1_1e, "5.1.1e", "trust in fss")
