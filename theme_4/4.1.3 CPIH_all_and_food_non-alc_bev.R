library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(tidyverse)
library(scales)
library(purrr)
library(lubridate)
library(countrycode)
library(mm23)


source(here::here("utils", "load-font.R"))
source(here::here("utils", "helpers-cpi_instant_brief.R"))

# download data ----------------------------------------------------------------
mm23 <- mm23::acquire_mm23()

metadata <- get_mm23_metadata(mm23)
mm23_month <- get_mm23_month(mm23)
weights <- get_weights(mm23, measure = "cpi")

cpih_all_yy <- "L55O"
cpih_food_yy <- "L55P"
cpih_all_mm <- "L59C"
cpih_food_mm <- "L59D"

cdids <- c(cpih_all_yy, cpih_food_yy)

af_duo_colours <- afcolours::af_colours("duo")

# CPIH -------------------------------------------------------------------------

cht <- line_chart(mm23_month, cdids)
t4_1_3_plot <- cht +
  #theme_ukfsr(base_family = "GDS Transport Website") +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  labs(y = "Year on year % change in CPIH") +
  scale_colour_manual(values=af_duo_colours, labels = c("Overall inflation", "Food inflation")) +
  scale_x_date(breaks = seq(min(mm23_month$date), max(mm23_month$date), by = "2 years"),date_labels = "%b %Y") +  # Adjust breaks to 3 years
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.text = element_text(margin = margin(r = 2, unit = 'cm')),
    plot.margin = margin(5,50,5,5,unit = "pt"))

save_graphic(t4_1_3_plot,"4.1.3a","cpih all and food non-alc bev")

# filter for relevant entries
t4_1_3 <- mm23_month |> 
filter(cdid == "L55O" | cdid == "L55P") 

# rename cells
t4_1_3$cdid[t4_1_3$cdid == "L55O"] <- "Overall inflation"
t4_1_3$cdid[t4_1_3$cdid == "L55P"] <- "Food inflation"

# deleted unwanted column
t4_1_3 <- t4_1_3[ -c(4) ]

# write out data to .CSV (and store in the bucket)
save_csv(t4_1_3,"4.1.3a","cpih all and food non-alc bev") 

# NOT USED Real terms prices ------------------------------------------------------------

#NEW SUPPORT 1: Changes in the food price index (real terms prices), 2010 to present, index (Chart)"
#Monthly data for Figure_14_6 on AUK
#specify L522: CPIH ALL ITEMS and L523: FOOD AND NON-ALCOHOLIC BEVERAGES to calculate real terms
CPI_month_real<-mm23_month %>% 
  filter(cdid=="D7BT" | cdid=="D7BU") %>% 
  arrange(cdid) %>% 
  pivot_wider(names_from=cdid,values_from=value) %>% 
  mutate(real_food_price=D7BU/D7BT*100) %>% #real value formula is nominal value/price index*100
  filter(as.Date("1999-12-01")<date & date<as.Date("2024-04-01")) %>%  #filter correct time series for plot <as.Date("2024-01-01")
  mutate(real_food_price=round(real_food_price,digits=1))

real_food_prices_plot <- CPI_month_real%>%
  ggplot(aes(x = date, y = `real_food_price`),
               value_name = "Index\n(2015=100)",legend_hide = TRUE  ) +
  geom_line() + 
  scale_y_continuous(limits=c(85,110),breaks= c(85,90,95,100,105,110),expand = expansion(c(0,0))) +
  scale_x_date(breaks=seq(as.Date("2000-01-01"),as.Date("2024-01-01"),by = "2 year"),labels=date_format("%Y"))+
  theme(panel.grid.major.x = element_blank(),
        axis.line.x = element_line(colour =  "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(0.25, "cm"))

real_food_prices_plot
  
# FSI Indicator 8b -------------------------------------------------------------

source(here::here("utils", "load-font.R"))
source(here::here("theme_4", "helpers-cpi.R"))


fsi8b <- mm23_month |> 
  filter(cdid %in% cdids, date >= "2014-01-01") |> 
  mutate(cdid = factor(cdid, levels = c("L55O", "L55P"), labels = c("All Items", "Food"))) |> 
  ggplot() +
  geom_line(aes(x= date, y = value, colour = cdid)) +
  scale_y_continuous(labels = label_percent(scale = 1,accuracy = 0.1),breaks = breaks_extended(10)) +
  scale_x_date(date_labels = "%b\n%Y") +
  scale_colour_manual(values = af_colours(type = "duo")) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")
  


# save_graphic(fsi8b, "fsi.8.1b", "cpih all and food non-alc bev fsi")
# save_csv(t4_1_3,"fsi.8.1b","cpih all and food non-alc bev fsi") 

for(i in c(16,22)) {
  
  cht <- fsi8b + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(50,50,5,5,unit = "pt"))
  
  save_graphic(cht, "fsi.8.1b", paste("cpih all and food non-alc bev fsi base", i))
  
}



