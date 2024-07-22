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


# FSI Indicator 6---------------------------------------------------------------
# Data is business investment in food drink and tobacco mfg, CVM SA £m,
# available here:
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ds4t/cxnv
# ONS CDID is DS4T

# Need to deal with revised data appearing as text
gms <- aws.s3::s3read_using(FUN = read_csv,
                                       bucket = ukfsr::s3_bucket(),
                                       object = "theme_3/t3_1_9/output/csv/3_1_9_grocery_market_share.csv")


barchart <- gms |> 
  filter(date == max(date)) |> 
  mutate(company = fct_reorder(company, value)) |> 
  ggplot() +
  geom_col(aes(x = company, y = value), fill = af_colours(n = 1)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = NULL, y = "Market share") +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE)

save_graphic(barchart, "3.1.10", "grocery market share latest")

---------------------------------------------------------------------------------------------------------------------------------------------------
  
# Support 1 - Year comparison 2021-2023


shops <- c("Symbols & Independent", "Ocado", "Other Outlets", "Iceland",
           "Waitrose", "Co-op", "Lidl", "Morrisons",
           "Aldi", "Asda", "Sainsbury's", "Tesco")


comp_barchart <- gms |> group_by(year, company) |> 
  summarise(avg = mean(value)) |> 
  filter(year %in% c(2021, 2023)) |> 
  mutate(company = fct(company, levels = shops), year = as.character(year)) |> 
  ggplot() +
  geom_col(aes(x = company, y = avg, fill = year), position = "dodge") +
  scale_fill_manual(values = af_colours(type = "duo")) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(horizontal = TRUE)


save_graphic(comp_barchart, "3.1.10a", "grocery market share comparison")
 