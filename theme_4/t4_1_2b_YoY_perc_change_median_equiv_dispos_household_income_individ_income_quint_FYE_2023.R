library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(scales)
library(data.table)

source(here("utils", "load-font.R"))

t_4_1_2b <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_4/t4_1_2/output/csv/4_1_2b_YoY_perc_change_median_equiv_dispos_household_income_individ_income_quint_FYE_2023.csv")

# https://stackoverflow.com/questions/11938293/how-to-label-a-barplot-bar-with-positive-and-negative-bars-with-ggplot2
# https://stackoverflow.com/questions/38268741/geom-bar-ggplot2-stacked-grouped-bar-plot-with-positive-and-negative-values-p
# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/

t_4_1_2b_long <- t_4_1_2b %>%
  gather(variable,value, "Quintile 1 median","Overall population median") 

af_categorical_colours <- afcolours::af_colours("duo")
names(af_categorical_colours)=levels(t_4_1_2b_long$variable)

# wrapping of a single word
# https://stackoverflow.com/questions/51515890/stringrstr-wrap-does-not-wrap-exactly-every-n-characters
t_4_1_2b_long$Year_wrap = str_replace_all(t_4_1_2b_long$Year, paste0("(.{5})"), "\\1\n")

t_4_1_2b_plot <- ggplot(t_4_1_2b_long, aes(x=Year_wrap, y=value, fill=factor(variable, levels=c("Quintile 1 median","Overall population median")))) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_text(data = subset(t_4_1_2b_long, variable == "Quintile 1 median"),
            aes(label=sprintf('%.1f',value),
                vjust=ifelse(value >= 0, -0.3, 1.4), hjust = 0.9), size=5) +
  geom_text(data = subset(t_4_1_2b_long, variable == "Overall population median"),
            aes(label=sprintf('%.1f',value),
               # vjust=ifelse(value >= 0, -0.3, 1.4), hjust = 0.4), size=6) +
                vjust=ifelse(value >= 0, -0.3, 1.4), hjust = ifelse(value >= 0, 0.1, 0.3)), size=5) +
  scale_y_continuous(limits = c(-3,5), breaks=seq(-3,5,1)) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 16) +
  scale_fill_manual(values = af_categorical_colours) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0),
    legend.title = element_blank(),
    legend.text = element_text(margin = margin(r = 3, unit = 'cm'))) +
  labs(y = "Percentage change (%)") +
  theme(axis.text.y =element_text(size = 24, margin=margin(0,20,0,20))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size=14)) +
  theme(legend.text=element_text(size=20)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(plot.margin = margin(t = 0,  # Top margin
                             r = 20,  # Right margin
                             b = 10,  # Bottom margin
                             l = 5)) + # Left margin
# avoid clipping
# https://stackoverflow.com/questions/38436996/label-gets-cut-on-ggplot2-bar-plot
  coord_cartesian(clip = "off")

t_4_1_2b_plot

save_graphic(t_4_1_2b_plot, "4.1.2b", "YoY perc change median equiv dispos household income individ income quint FYE 2023")

save_csv(t_4_1_2b, "4.1.2b", "YoY perc change median equiv dispos household income individ income quint FYE 2023")

