library(here)
library(dplyr)
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

source(here::here("utils", "load-font.R"))

# download data ----------------------------------------------------------------
mm23 <- mm23::acquire_mm23()

# data files--------------------------------------------------------------------
metadata <- get_mm23_metadata(mm23)
mm23_month <- get_mm23_month(mm23)
weights <- get_weights(mm23, measure = "cpi")

# Plot
af_colours_1 <- c(
  "#12436D" # Dark blue
)

# INDICES
# specify L522: CPIH ALL ITEMS and L523: FOOD AND NON-ALCOHOLIC BEVERAGES to calculate real terms
t_4_1_3b <-mm23_month %>% 
  filter(cdid=="L522" | cdid=="L523") %>% 
  arrange(cdid) %>% 
  pivot_wider(names_from=cdid,values_from=value) %>% 
  mutate(real_food_price=L523/L522*100) %>% #real value formula is nominal value/price index*100
  filter(as.Date("1999-12-01")<date & date<as.Date("2024-09-01")) %>%  #filter correct time series for plot <as.Date("2024-01-01")
  mutate(real_food_price=round(real_food_price,digits=1)) %>%
  select(!c("period"))  %>%
  rename(`CPIH - All Items` = L522) %>%
  rename(`CPIH - Food and non-alcoholic beverages` = L523) %>%
  rename(`CPIH - All Items - real terms` = real_food_price)

t_4_1_3b_plot <- ggplot(t_4_1_3b, aes(x = date, y = `CPIH - All Items - real terms`)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  geom_line(linewidth=1, colour = af_colours_1) +  
  scale_y_continuous(limits=c(85,110),breaks= c(85,90,95,100,105,110),expand = expansion(c(0,0))) +
  scale_x_date(breaks=seq(as.Date("2000-01-01"),as.Date("2024-08-01"),by = "2 year"),labels=date_format("%Y"))+
  theme(panel.grid.major.x = element_blank(),
        axis.line.x = element_line(colour =  "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm")) +
  theme(
    axis.text.x = element_text(size=22)) +
  theme(axis.title.x = element_blank()) +
  labs(x = "Year", y = "Index (2015 = 100)")

t_4_1_3b_plot


save_graphic(t_4_1_3b_plot, "4.1.3b", "changes CPIH food price index real terms prices")

save_csv(t_4_1_3b, "4.1.3b", "changes CPIH food price index real terms prices")


