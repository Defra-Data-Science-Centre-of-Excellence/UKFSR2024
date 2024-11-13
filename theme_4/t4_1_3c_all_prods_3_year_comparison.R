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
library(scales)
library(knitr)
library(data.table)

source(here::here("utils", "load-font.R"))

# download data ----------------------------------------------------------------
mm23 <- mm23::acquire_mm23()

# data files--------------------------------------------------------------------
metadata <- get_mm23_metadata(mm23)
mm23_month <- get_mm23_month(mm23)
weights <- get_weights(mm23, measure = "cpi")

t_4_1_3c_long <-mm23_month %>% 
  # filter(cdid=="L522" | cdid=="L523" | cdid=="L52I" | cdid=="L52J" | cdid=="L52K" | cdid=="L52L" | cdid=="L52M" | cdid=="L52N" |
  # cdid=="L52O" | cdid=="L52P" | cdid=="L52Q" | cdid=="L52S" | cdid=="L52T" | cdid=="L52R" | cdid=="L52U" | cdid=="L52V" |
  # cdid=="L52W" | cdid=="L52X") %>%
  # edited version
  filter(cdid=="L522" | cdid=="L523" | cdid=="L52I" | cdid=="L52J" | cdid=="L52K" | cdid=="L52L" | cdid=="L52N" |
           cdid=="L52O") %>%
  arrange(cdid) %>% 
  filter(date == as_date("2021-08-01") | 
           date == as_date("2024-08-01")) %>%
  group_by(date) %>%
  mutate(real = (value/value[cdid == "L522"])*100) %>%
  group_by(cdid) %>%
  mutate(three_yr_pct_change = (real - lag(real))/lag(real) * 100) %>%
  filter(date == as_date("2024-08-01")) %>%
  filter(cdid != "L522") %>%
  select(!c("date","value","real","period"))  %>%
  dplyr::rename(`Percentage change (%)` = three_yr_pct_change) %>%
  #  transform(cdid=gsub(pattern="L522", replacement="All Items", cdid)) %>%
  transform(cdid=gsub(pattern="L523", replacement="Food and non-alcoholic beverages", cdid)) %>%
  transform(cdid=gsub(pattern="L52I", replacement="Bread and cereals", cdid)) %>%  
  transform(cdid=gsub(pattern="L52J", replacement="Meat", cdid)) %>%  
  transform(cdid=gsub(pattern="L52K", replacement="Fish", cdid)) %>%  
  transform(cdid=gsub(pattern="L52L", replacement="Milk, cheese and eggs", cdid)) %>%  
  # transform(cdid=gsub(pattern="L52M", replacement="Oils and fats", cdid)) %>%
  transform(cdid=gsub(pattern="L52N", replacement="Fruit", cdid)) %>%  
  transform(cdid=gsub(pattern="L52O", replacement="Vegetables, including potatoes and tubers", cdid)) %>%  
  # transform(cdid=gsub(pattern="L52P", replacement="Sugar, jam, honey, syrups, chocolate and confectionery", cdid)) %>%
  # transform(cdid=gsub(pattern="L52Q", replacement="Food products (nec)", cdid)) %>%
  # transform(cdid=gsub(pattern="L52S", replacement="Coffee, tea amd cocoa", cdid)) %>%
  # transform(cdid=gsub(pattern="L52T", replacement="Mineral waters, soft drinks and juices", cdid)) %>%
  # transform(cdid=gsub(pattern="L52R", replacement="Non-alcoholic beverages", cdid)) %>%
  # transform(cdid=gsub(pattern="L52U", replacement="Alcoholic beverages", cdid)) %>%
  # transform(cdid=gsub(pattern="L52V", replacement="Spirits", cdid)) %>%
  # transform(cdid=gsub(pattern="L52W", replacement="Wine", cdid)) %>%
  # transform(cdid=gsub(pattern="L52X", replacement="Beer", cdid)) %>%
  dplyr::rename(`Food group` = cdid)

# includes line for labelling just one bar to 3 decimal places
# https://stackoverflow.com/questions/66469336/i-cant-get-label-sprintf-to-force-my-labels-to-show-2-decimal-places
# for labelling just one bar to 3 decimal places
# https://stackoverflow.com/questions/66469336/i-cant-get-label-sprintf-to-force-my-labels-to-show-2-decimal-places
t_4_1_3c_plot <- ggplot(t_4_1_3c_long, aes(x=reorder(`Food group`, `Percentage change (%)`), y=`Percentage change (%)`,
                                           fill=factor(ifelse(`Food group` == "Food and non-alcoholic beverages","Highlighted","Normal")))) +
  geom_bar(stat="identity", show.legend = FALSE, width=0.5) +
  geom_text(aes(label = ifelse(`Food group` == "Food and non-alcoholic beverages", sprintf('%.1f',`Percentage change (%)`), 
                               sprintf('%.1f',`Percentage change (%)`)), hjust=ifelse(`Percentage change (%)` < 0, 1.3, -0.4), vjust = 0.5), 
            size=8, parse = FALSE, family = "GDS Transport Website") +
  scale_y_continuous(limits = c(-5,20), breaks=seq(-5,20,5)) +
  scale_x_discrete(labels = label_wrap_gen(width = 25)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14, horizontal = TRUE) +
  scale_fill_manual(name = "`Food group`", values=c("#F46A25","#12436D")) +
  theme(axis.title.y = element_blank())  +
  coord_flip()  +
  xlab( "Three yearly % change in CPIH") +
  theme(plot.margin = unit(c(t = 1,  # Top margin
                             r = 1,  # Right margin
                             b = 1,  # Bottom margin
                             l = 2), 'cm'))  # Left margin

t_4_1_3c_plot

save_graphic(t_4_1_3c_plot, "4.1.3c", "all prods 3 year comparison")

save_csv(t_4_1_3c_long, "4.1.3c", "all prods 3 year comparison")

