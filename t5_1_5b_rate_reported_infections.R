library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

setwd("~/shiny/UKFSR2024/files")

source("colours.R")
source("load-font.R")

t_5_1_5b <- fread("5_1_5b_rate_reported_infections_per_pop_per_year.csv")

############################################################################################################################################
# https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y
# https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
# https://stackoverflow.com/questions/20037843/how-to-use-italics-for-facet-labels-in-ggplot2

t_5_1_5b$Year <- factor(t_5_1_5b$Year, levels = c("2015","2016","2017","2018","2019","2020","2021","2022"))

t_5_1_5b_long <- t_5_1_5b %>% 
  group_by(Year) %>%
  pivot_longer(cols=c("Campylobacter sp.","Non typhoidal Salmonella sp.","STEC O157","Listeria monocytogenes"),
               names_to="species",
               values_to="value")

gcols= af_categorical_colours1[c(1)]

t5_1_1g_long$Wave_wrap = str_wrap(t5_1_1g_long$Wave, width = 14)

# # https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# # ensure question axis matches original vector
# # https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
# # Turn question column into a character vector
# # Then turn it back into a factor with the levels in the correct order
t5_1_1g_long$Wave_wrap <- factor(t5_1_1g_long$Wave_wrap, levels=unique(t5_1_1g_long$Wave_wrap))

t_5_1_5b_long <- setDT(t_5_1_5b_long)

t_5_1_5b_long[,y_min := 0]
t_5_1_5b_long[,y_max:= value*1.1, by = species]

  t_5_1_5b_long_plot <- ggplot(t_5_1_5b_long, aes(x=factor(Year), y=value, group=species)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  geom_line(linewidth=1, colour = gcols) +
  labs(x = "Year", y = str_wrap("Number of UK laboratory confirmed cases per 100,000 population", width = 40)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.title.x=element_text(size=20)) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 1, hjust=1)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(plot.title = element_text(size=14, face="italic")) +
  theme(legend.position = "none")

t_5_1_5b_long_plot_facet <- t_5_1_5b_long_plot + facet_wrap(~ species, ncol=2, scales = "free_y") +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  theme(strip.background = element_blank(), strip.placement = "outside") 

t_5_1_5b_long_plot_facet

ggsave(filename = "5_1_5b_rate_reported_infections_per_pop_per_year.svg",
       t_5_1_5b_long_plot_facet,
       width = 960,
       height = 640,
       units = "px",
       dpi = 72)

