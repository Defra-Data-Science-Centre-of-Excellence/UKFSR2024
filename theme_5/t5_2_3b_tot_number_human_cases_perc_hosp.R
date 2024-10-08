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
library(forcats)

source(here::here("utils", "load-font.R"))

t5_2_3b_labels <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_5/t5_2_3/output/csv/5_2_3b_tot_number_human_cases_perc_hosp.csv")

t5_2_3b_labels$`Causative agent` <- factor(t5_2_3b_labels$`Causative agent`, levels = c("Salmonella spp.","Enteric viruses[1]","Campylobacter spp.",
"Clostridium perfringens","STEC & DEC","Listeria monocytogenes",
"Shigella spp.","Cryptosporidium spp.","Other[2]","Unknown[3]","Total"))

# convert to long form, bringing in separate data and label columns
# https://stackoverflow.com/questions/59253987/parallel-pivot-longer-of-two-sets-of-columns

t5_2_3b_long_labels <- t5_2_3b_labels %>% 
  pivot_longer(-`Causative agent`, 
               names_to = c("Year", ".value"), 
               names_sep="_" )

t5_2_3b_long_labels$Year <- factor(t5_2_3b_long_labels$Year, levels = c("2019","2020","2021","2022","2023","Total"))

level_order <- c("2019","2020","2021","2022","2023","Total")

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_2_3b_long_labels$`Causative agent_wrap` = str_wrap(t5_2_3b_long_labels$`Causative agent`, width = 14)

t5_2_3b_labels$`Causative agent_wrap` <- factor(t5_2_3b_labels$`Causative agent_wrap`, levels = c("Salmonella spp.","Enteric viruses[1]","Campylobacter spp.",
                                                                          "Clostridium perfringens","STEC & DEC","Listeria monocytogenes",
                                                                          "Shigella spp.","Cryptosporidium spp.","Other[2]","Unknown[3]","Total"))

t5_2_3b_plot <- ggplot(t5_2_3b_long_labels1,aes(x=Year, y=val)) +
  geom_bar(stat="identity", fill = af_colours_1, position = position_dodge(width=2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_x_discrete(breaks = 2019:2023) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values=af_colours_1) +
  theme(legend.position = "none") +
  labs(y = "Number of human cases (% hospitalised)") +
  theme(
    axis.text.x = element_text(size=18),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  geom_text(aes(label = ifelse(val>0, label, "")), color="white", fontface = "bold", size=4, vjust = 1.5,) +
  facet_wrap(~ factor(`Causative agent`,levels = c("Salmonella spp.","Enteric viruses[1]","Campylobacter spp.",
                                                   "Clostridium perfringens","STEC & DEC","Listeria monocytogenes",
                                                   "Shigella spp.","Cryptosporidium spp.","Other[2]","Unknown[3]","Total")), 
             ncol = 3, scales = "free", drop=FALSE) +
  theme(
    legend.position="none",
    strip.text = element_text(size = 24, face = "bold"),
    strip.background =element_rect(fill="white"))

t5_2_3b_plot

save_graphic(t5_2_3b_plot, "5.2.3b", "tot number human cases perc hosp facet")

save_csv(t5_2_3b_labels, "5.2.3b", "tot number human cases perc hosp facet")

