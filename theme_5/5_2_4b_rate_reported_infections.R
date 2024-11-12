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

t_5_2_4b <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_5/t5_2_4/output/csv/5_2_4b_rate_reported_infections_per_pop_per_year.csv")

############################################################################################################################################
# https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y
# https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
# https://stackoverflow.com/questions/20037843/how-to-use-italics-for-facet-labels-in-ggplot2

t_5_2_4b$Year <- factor(t_5_2_4b$Year, levels = c("2019","2020","2021","2022","2023"))

t_5_2_4b_long <- t_5_2_4b |> 
  group_by(Year) |>
  pivot_longer(cols=c("Campylobacter sp.","Non typhoidal Salmonella sp.","STEC O157","Listeria monocytogenes"),
               names_to="species",
               values_to="value")

t_5_2_4b_long <- setDT(t_5_2_4b_long)

t_5_2_4b_long[,y_min := 0]
t_5_2_4b_long[,y_max:= value*1.1, by = species]

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t_5_2_4b_long_plot <- ggplot(t_5_2_4b_long, aes(x=factor(Year), y=value, group=species)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  geom_line(linewidth=1, colour = af_colours_1) +
  labs(x = "Year", y = str_wrap("Number of UK laboratory confirmed cases per 100,000 population", width = 40)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm"))

t_5_2_4b_long_plot_facet <- t_5_2_4b_long_plot + facet_wrap(~ species, ncol=2, scales = "free_y") +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  theme(strip.background = element_blank(), strip.placement = "outside") 

t_5_2_4b_long_plot_facet

save_graphic(t_5_2_4b_long_plot_facet, "5.2.4b", "rate reported infections per pop per year")

save_csv(t_5_2_4b_long, "5.2.4b", "rate reported infections per pop per year")
