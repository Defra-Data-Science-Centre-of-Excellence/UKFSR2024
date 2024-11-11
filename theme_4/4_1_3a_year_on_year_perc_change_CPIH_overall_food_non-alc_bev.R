library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(janitor)
library(scales)
library(purrr)
library(lubridate)
library(countrycode)
library(mm23)
library(ukfsr)
library(data.table)
library(zoo)
library(afcolours)

source(here("utils", "load-font.R"))

# download data ----------------------------------------------------------------
mm23 <- mm23::acquire_mm23()

# data files--------------------------------------------------------------------
metadata <- get_mm23_metadata(mm23)
mm23_month <- get_mm23_month(mm23)
weights <- get_weights(mm23, measure = "cpi")

# CPIH INFLATION
cdids <- c("L55O", "L55P")

t_4_1_3a_long <- mm23_month %>% 
  filter(cdid=="L55O" | cdid=="L55P") %>% 
  arrange(cdid) %>% 
  select(!c("period"))  %>%
  filter(as.Date("2004-07-01") < date & date < as.Date("2024-09-01")) %>%
  transform(cdid=gsub(pattern="L55O", replacement="CPIH - Overall inflation", cdid)) %>%
  transform(cdid=gsub(pattern="L55P", replacement="CPIH - Food inflation", cdid)) 

t_4_1_3a_long$date <- zoo::as.yearmon(t_4_1_3a_long$date, "%m %Y")

af_categorical_colours_2 <- afcolours::af_colours("duo")
names(af_categorical_colours_2)=levels(t_4_1_3a_long$cdid)

t_4_1_3a_plot <- ggplot(t_4_1_3a_long, aes(x=date, y=value, colour=cdid)) +
  geom_line(linewidth=1) +
  scale_colour_manual(values = af_categorical_colours_2) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(y = "Year on year % change in CPIH") +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(margin = margin(r = 2, unit = 'cm'))
  ) +
  theme(axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 10,  # Top margin
                             r = 40,  # Right margin
                             b = 10,  # Bottom margin
                             l = 5))  # Left margin

t_4_1_3a_plot

save_graphic(t_4_1_3a_plot, "4.1.3a", "year on year perc change CPIH overall food non alc bev")

save_csv(t_4_1_3a_long, "4.1.3a", "year on year perc change CPIH overall food non alc bev")

