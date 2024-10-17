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

t5_2_2d <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_2_2/output/csv/5_2_2d_Total_number_of_food_alerts_issued_by_the_UK_during_FYE_2016_to_FYE_2024_Reporting_Years.csv")

t5_2_2d$Alert <- factor(t5_2_2d$Alert, levels = c("AA","PRIN","FAFA"))


t5_2_2d_long <- t5_2_2d %>% 
  group_by(Alert) %>%
  pivot_longer(cols=c("FYE 2016","FYE 2017","FYE 2018","FYE 2019","FYE 2020","FYE 2021","FYE 2022","FYE 2023","FYE 2024"),
               names_to="Year",
               values_to="Value")

t5_2_2d_long$Year <- factor(t5_2_2d_long$Year, levels = c("FYE 2016","FYE 2017","FYE 2018","FYE 2019","FYE 2020","FYE 2021",
                                                          "FYE 2022","FYE 2023","FYE 2024"))

level_order <- c("FYE 2016","FYE 2017","FYE 2018","FYE 2019","FYE 2020","FYE 2021",
                 "FYE 2022","FYE 2023","FYE 2024")

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(t5_2_2d_long$Alert)

  t5_2_2d_plot <- ggplot(t5_2_2d_long,aes(x=Year, y=Value, label = round(Value,0), group=Alert)) +
  geom_bar(stat="identity", aes(fill = Alert)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  labs(y = "Total number of food alerts") +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size=20)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.x = element_blank()) +
  geom_text(stat = "stratum", aes(stratum = Alert), color="white", fontface = "bold", size=6, vjust = -0.8,
            nudge_y = c(-2, 0, -1.5, -2, -2, 0, 0, 0, -1))
  
t5_2_2d_plot


save_graphic(t5_2_2d_plot, "5.2.2d", "Total number of food alerts issued by the UK during FYE 2016 to FYE 2024 Reporting Years")

save_csv(t5_2_2d, "5.2.2d", "Total number of food alerts issued by the UK during FYE 2016 to FYE 2024 Reporting Years")

