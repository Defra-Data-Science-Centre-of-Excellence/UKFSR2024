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
                                object = "theme_5/t5_1_1/output/csv/5_1_1d_trust_FSA.csv")

t5_1_1d$Wave <- factor(t5_1_1d$Wave, levels = c("Wave 1 (07/2020 - 10/2020)","Wave 2 (11/2020 - 01/2021)","Wave 3 (04/2021 - 06/2021)",
                                                "Wave 4 (10/2021 - 01/2022)","Wave 5 (04/2022 - 07/2022)","Wave 6 (10/2022 - 01/2023)",
                                                "Wave 7 (04/2023 - 07/2023)"))

t5_1_1d_long <- t5_1_1d %>% 
  group_by(Wave) %>%
  pivot_longer(cols=c("Trust","Neither trust nor distrust","Distrust","Don't know"),
               names_to="Response",
               values_to="Value")

t5_1_1d_long$Response <- factor(t5_1_1d_long$Response, levels = c("Trust","Neither trust nor distrust","Distrust","Don't know"))

level_order <- c("Trust","Neither trust nor distrust","Distrust","Don't know")

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(t5_1_1d_long$Response)

t5_1_1d_long$Wave_wrap = str_wrap(t5_1_1d_long$Wave, width = 14)


t5_1_1d_plot <- ggplot(t5_1_1d_long,aes(x=Wave_wrap, y=Value, label = round(Value,0), group=Response)) +
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

t5_1_1d_plot

save_graphic(t5_1_1d_plot, "5.1.1d", "trust in fsa")

save_csv(t5_1_1d, "5.1.1d", "trust in fsa")
