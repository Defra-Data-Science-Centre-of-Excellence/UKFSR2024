library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(data.table)

source(here("utils", "load-font.R"))

t4_1_5b <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_4/t4_1_5/output/csv/4_1_5b_food_bank_usage_by_age_of_head_of_household.csv")

t4_1_5b$`Age group` <- factor(t4_1_5b$`Age group`, levels = c("16 to 24","25 to 34","35 to 44","45 to 54","55 to 59","60 to 64","65 to 74",
                                                                    "75 to 84","85 and over"))

t4_1_5b_long <- t4_1_5b |> 
  group_by(`Age group`) |>
  pivot_longer(cols=c("30 day food bank usage","12 month food bank usage"),
               names_to="Usage",
               values_to="Value")

t4_1_5b_long$Usage <- factor(t4_1_5b_long$Usage, levels = c("30 day food bank usage","12 month food bank usage"))

level_order <- c("30 day food bank usage","12 month food bank usage")

af_categorical_colours <- afcolours::af_colours("duo")
names(af_categorical_colours)=levels(t4_1_5b_long$Usage)

t4_1_5b_long$Value[t4_1_5b_long$Value == 0] <- NA

# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/
t4_1_5b_plot <- ggplot(t4_1_5b_long, aes(x=factor(`Age group`), y=Value, fill=Usage)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits = c(0,8), breaks=seq(0,8,1)) +
  geom_text(data = subset(t4_1_5b_long, Usage == "30 day food bank usage"),
            aes(label=Value, vjust = -0.5, hjust = 2.3), size=8) +
  geom_text(data = subset(t4_1_5b_long, Usage == "12 month food bank usage"),
            aes(label=Value, vjust = -0.5, hjust = -1.4), size=8) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)
  ) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size=20)) +
  theme(legend.text=element_text(size=24)) +
  labs(y = "Percentage of households (%)") +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") 

t4_1_5b_plot

save_graphic(t4_1_5b_plot, "4.1.5b", "food bank usage by age of head of household")

save_csv(t4_1_5b, "4.1.5b", "food bank usage by age of head of household")

