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
library(gridExtra)
library(ggpubr)
library(ggplotify)
library(data.table)

source(here("utils", "load-font.R"))

t5_1_1g <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_fsi/tfsi_9_1/output/csv/fsi_9_1_fsa_respond_confid_food_supply_chain_actors_ensure_food_safe_eat_in.csv")

t5_1_1g$Wave <- factor(t5_1_1g$Wave, levels = c("Wave 1 (07/2020 - 10/2020)","Wave 2 (11/2020 - 01/2021)","Wave 3 (04/2021 - 06/2021)",
                                                "Wave 4 (10/2021 - 01/2022)","Wave 6 (10/2022 - 01/2023)","Wave 7 (04/2023 - 07/2023)"))

t5_1_1g_long <- t5_1_1g |> 
  group_by(Wave) |>
  pivot_longer(cols=c("Food delivery services","Take-aways","Slaughterhouses and dairies","Food manufacturers","Restaurants","Shops and supermarkets",
                      "Farmers"),
               names_to="Actor",
               values_to="Value")

t5_1_1g_long$Actor <- factor(t5_1_1g_long$Actor, levels = c("Farmers","Slaughterhouses and dairies","Food manufacturers","Shops and supermarkets",
                                                            "Restaurants","Take-aways","Food delivery services"))

level_order <- c("Farmers","Slaughterhouses and dairies","Food manufacturers","Shops and supermarkets","Restaurants","Take-aways","Food delivery services")

# create new copied column
t5_1_1g_long$Wave_label  = t5_1_1g_long$Wave 
t5_1_1g_long$Wave_label <- as.character(t5_1_1g_long$Wave_label)

# rename facet x axis labels
t5_1_1g_long <- t5_1_1g_long |> 
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 1 (07/2020- 10/2020)", "W1")) |>
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 2 (11/2020- 01/2021)", "W2")) |>
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 3 (04/2021- 06/2021)", "W3")) |>
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 4 (10/2021- 01/2022)", "W4")) |>
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 6 (10/2022- 01/2023)", "W6")) |>
  mutate(Wave_label = replace(Wave_label, Wave_label == "Wave 7 (04/2023- 07/2023)", "W7"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_1_1g_plot <- ggplot(t5_1_1g_long, aes(x=factor(Wave_label), y=Value)) +
  geom_bar(stat="identity", fill = af_colours_1, position = position_dodge(width=2)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values=af_colours_1) +
  theme(legend.position = "none") +
  labs(y = "Percentage of respondents (%)") +
  theme(
    axis.text.x = element_text(size=28),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.background = element_blank()) +
  geom_text(aes(label = round(Value,0)), size = 10, vjust = 1.5, fontface = "bold", colour = "white") +
  facet_wrap(~ Actor, ncol = 3, scales = "free_x", drop=FALSE) +
  theme(
    legend.position="none",
    strip.text = element_text(size = 28, face = "bold"),
    strip.background =element_rect(fill="white"))

# create text box
label <- ("**Key:** <br> <br>
           W1 = Wave 1 (07/2020 - 10/2020) <br> 
           W2 = Wave 2 (11/2020 - 01/2021) <br>
           W3 = Wave 3 (04/2021 - 06/2021) <br>
           W4 = Wave 4 (10/2021 - 01/2022) <br>
           W6 = Wave 6 (10/2022 - 01/2023) <br>
           W7 = Wave 7 (04/2023 - 07/2023)")

grid.newpage()
t5_1_1g_plot

textbox <- textbox_grob(
  label,
  x = unit(0.89, "npc"), y = unit(0.17, "npc"),
  gp = gpar(fontsize = 22),
  box_gp = gpar(col = "black"),
  r = unit(5, "pt"),  
  padding = unit(c(10, 0, 7, 10), "pt"),
  margin = unit(c(10, 1075, 0, 20), "pt")
)

grid.draw(textbox)

# AT THE MOMENT, THE ONLY WAY TO SAVE THIS COMBINED GRAPHIC IS TO ZOOM IN ON THE IMAGE, RIGHT CLICK AND SAVE (AS A PNG)
# FOR BUCKET, RENAME FILE AS 'fsi_9_1_FSA_respond_confid_food_supply_chain_actors_ensure_food_safe_eat_in_PDF_version.png'
