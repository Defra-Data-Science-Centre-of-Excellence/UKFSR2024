library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)

source(here::here("utils", "load-font.R"))

cyber <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/cyber_security_breaches.csv")

cht <- ggplot(cyber, aes(x = year, y = value, colour = sector)) +
  geom_line(data = filter(cyber, year >= 2022 & sector == "Businesses"), linetype = "dotted") +
  geom_line(data = filter(cyber, year <= 2022 & sector == "Businesses"), linetype = "solid") +
  geom_line(data = filter(cyber, sector == "Charities"), linetype = "solid") +
  # geom_text(aes(label = paste0(value, "%")), vjust = -1.5, size = 8) +
  scale_colour_manual(values = af_colours("duo")) +
  scale_y_continuous(limits = c(0,50), labels = scales::label_percent(scale = 1), expand = expansion(mult = c(0,0.05))) +
  scale_x_continuous(breaks = seq(2017, 2023, by = 3)) +
  labs(x= NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

# Chart notes
# Bases: 1000+ UK businesses each year; 300+ charities per year 
# The weighting approach for businesses was changed for 2020, although this is
# expected to have a negligible impact on comparability to previous years
# 
# The sample frame for businesses was changed in 2023, although it is still
# intended to produce a representative sample of businesses. We have therefore
# used a dotted line for 2023 business trends

save_graphic(cht, "3.3.1a", "cyber security breaches")
save_csv(cyber, "3.3.1a", "cyber security breaches")
