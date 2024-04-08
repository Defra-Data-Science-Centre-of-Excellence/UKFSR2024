library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

# Data is business investment in food drink and tobacco mfg, CVM SA £m,
# available here:
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ds4t/cxnv
# ONS CDID is DS4T

# Need to deal with revised data appearing as text
mfg_investment <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_3/t3_1_99/output/csv/3_1_99_food_mfg_investment_cdid_ds4t.csv")


mfg_investment <- mfg_investment |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01"))) |> 
  filter(year >= 2014)


chart <- mfg_investment |> 
  ggplot() +
  geom_line(aes(x = date, y = value), 
            colour = af_colours(type = "categorical", n = 1),
            lwd = 1) +
  scale_y_continuous(limits = c(0, 1400)) +
  labs(x = NULL,
       y = "£m") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(chart, "3.1.99", "food mfg investment fsi")
