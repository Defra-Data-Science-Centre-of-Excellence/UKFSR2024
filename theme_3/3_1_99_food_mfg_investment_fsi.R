library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))


# FSI Indicator 6---------------------------------------------------------------
# Data is business investment in food drink and tobacco mfg, CVM SA £m,
# available here:
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/ds4t/cxnv
# ONS CDID is DS4T

# Need to deal with revised data appearing as text
mfg_investment <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_fsi/tfsi_6_1/output/csv/fsi_6_1_food_mfg_investment_cdid_ds4t.csv")


mfg_investment <- mfg_investment |> 
  mutate(date = as.Date(paste0(year, "-", ((quarter - 1) * 3 + 1), "-01"))) |> 
  filter(year >= 2014)


chart <- mfg_investment |> 
  ggplot() +
  geom_line(aes(x = date, y = value), 
            colour = af_colours(type = "categorical", n = 1)) +
  scale_y_continuous(limits = c(0, 1400), labels = scales::label_comma()) +
  labs(x = NULL,
       y = "£m") +
  theme_ukfsr(base_family = "GDS Transport Website")

# save_graphic(chart, "fsi.6.1", "food mfg investment fsi")


for(i in c(16,22)) {
  
  cht <- chart + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt"))+
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.6.1", paste("food mfg investment fsi base", i))
  
}

