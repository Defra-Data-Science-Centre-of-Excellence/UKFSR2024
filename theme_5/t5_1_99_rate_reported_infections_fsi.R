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

source(here::here("utils", "load-font.R"))

t_5_1_5b <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_fsi/tfsi_9_1/output/csv/fsi_9_1_rate_reported_infections_per_pop_per_year.csv")

############################################################################################################################################
# https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y
# https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
# https://stackoverflow.com/questions/20037843/how-to-use-italics-for-facet-labels-in-ggplot2

t_5_1_5b$Year <- factor(t_5_1_5b$Year, levels = c("2015","2016","2017","2018","2019","2020","2021","2022"))

t_5_1_5b_long <- t_5_1_5b |> 
  group_by(Year) |>
  pivot_longer(cols=c("Campylobacter sp.","Non typhoidal Salmonella sp.","STEC O157","Listeria monocytogenes"),
               names_to="species",
               values_to="value")

t_5_1_5b_long <- setDT(t_5_1_5b_long)

t_5_1_5b_long[,y_min := 0]
t_5_1_5b_long[,y_max:= value*1.1, by = species]

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t_5_1_5b_long_plot <- ggplot(t_5_1_5b_long, aes(x=factor(Year), y=value, group=species)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  geom_line(linewidth=1, colour = af_colours_1) +
  labs(x = "Year", y = str_wrap("Number of UK laboratory confirmed cases per 100,000 population", width = 40)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 

t_5_1_5b_long_plot_facet <- t_5_1_5b_long_plot + facet_wrap(~ species, ncol=2, scales = "free_y") +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  theme(strip.background = element_blank(), strip.placement = "outside") 

t_5_1_5b_long_plot_facet

save_graphic(t_5_1_5b_long_plot_facet, "fsi.9.1", "rate reported infections per pop per year fsi")


