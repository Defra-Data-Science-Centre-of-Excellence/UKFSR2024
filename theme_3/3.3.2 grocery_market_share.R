#devtools::install_github("FoodchainStats/ukfsr")


library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)

source(here::here("utils", "load-font.R"))


gms <- s3read_using(FUN = read_csv,
                    bucket = ukfsr::s3_bucket(),
                    object = "theme_3/input_data/grocery_market_share.csv")

# NOT USED Year comparison 2021-2023-----------------------------------------------------
# Poss could switch to 2024 at the last minute if we can get nearly a whole year
# of data

shops <- c("Symbols & Independent", "Ocado", "Other Outlets", "Iceland",
           "Waitrose", "Co-op", "Lidl", "Morrisons",
           "Aldi", "Asda", "Sainsbury's", "Tesco")


comp_barchart <- gms |> group_by(year, company) |> 
  summarise(avg = mean(value)) |> 
  filter(year %in% c(2021, 2023)) |> 
  mutate(company = fct(company, levels = shops),
         year = fct(as.character(year), levels = c("2023", "2021"))) |> 
  ggplot() +
  geom_col(aes(x = company, y = avg, fill = year), position = "dodge") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = af_colours(type = "duo")) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE)) +
  coord_flip() +
  labs(x = NULL, y = "Market share") +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) + 
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.15))


# save_graphic(comp_barchart, "3.4.1a", "grocery market share comparison")


# NOT USED Alternate showing the latest data---------------------------------------------

barchart <- gms |> 
  filter(date == max(date)) |> 
  mutate(company = fct_reorder(company, value)) |> 
  ggplot() +
  geom_col(aes(x = company, y = value), fill = af_colours(n = 1)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = NULL, y = "Market share") +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE)

# save_graphic(barchart, "3.1.10", "grocery market share latest")

# USED Facet chart -------------------------------------------------------------------


library(gghighlight)

gms |> 
  mutate(company = fct(company, levels = rev(shops))) |> 
  ggplot() +
  geom_line(aes(x = date, y = value, colour = company), colour = af_colours()[1]) +
  gghighlight(value >= 0,use_direct_label = FALSE) +
  facet_wrap(vars(company)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr() + theme(legend.position = "none")

shops_trunc <- c("Other",
           "Waitrose", "Co-op", "Lidl", "Morrisons",
           "Aldi", "Asda", "Sainsbury's", "Tesco")

facet_cht <- gms |> 
  mutate(company = case_when(company %in% c("Iceland", "Other Outlets", "Ocado", "Symbols & Independent") ~ "Other",
                   .default = company),
         company = fct(company, levels = rev(shops_trunc))) |> 
  group_by(date, company) |> 
  summarise(value = sum(value)) |> 
  
  
  ggplot() +
  geom_line(aes(x = date, y = value, colour = company), colour = af_colours()[1]) +
  gghighlight(value >= 0,use_direct_label = FALSE) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_x_date(date_labels = "%Y",) +
  facet_wrap(vars(company)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "none", plot.margin = margin(r = 1, unit = "cm"))

save_graphic(facet_cht, "3.3.2a", "grocery market share time series")
# csv not saved because Im not sure we should be disseminating Kantar data