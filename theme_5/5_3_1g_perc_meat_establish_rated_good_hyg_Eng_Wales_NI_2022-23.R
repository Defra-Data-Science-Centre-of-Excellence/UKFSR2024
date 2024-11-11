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

t5_3_1g <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_3_1/output/csv/5_3_1g_perc_meat_establish_rated_good_hyg_Eng_Wales_NI_2022-23.csv")

t5_3_1g_long <- t5_3_1g %>%
  gather(variable,value,`England and Wales`,`Northern Ireland`) 

af_categorical_colours <- afcolours::af_colours("duo")
names(af_categorical_colours)=levels(t5_3_1g_long$variable)

# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/
t5_3_1g_plot <- ggplot(t5_3_1g_long, aes(x=factor(year), y=value, fill=variable)) +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.9)) +
  geom_text(data = subset(t5_3_1g_long, variable == "England and Wales"),
            aes(label=sprintf("%.1f", value)), vjust= 1.5, hjust = 2.5, size = 8, fontface = "bold", colour = "white") +
  geom_text(data = subset(t5_3_1g_long, variable == "Northern Ireland"),
            aes(label=sprintf("%.1f", value)), vjust= 1.5, hjust = -1.1, size = 8, fontface = "bold", colour = "white") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)
  ) +
  theme(axis.title.x=element_blank()) +
  labs(x = "Year") +
  labs(y = "Percentage rated as good or generally satisfactory (%)") # +

t5_3_1g_plot

save_graphic(t5_3_1g_plot, "5.3.1g", "perc meat establish rated good hyg Eng Wales NI_2022-23")

save_csv(t5_3_1g, "5.3.1g", "perc meat establish rated good hyg Eng Wales NI_2022-23")

