library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

tfp <- aws.s3::s3read_using(FUN = read_csv,
                                       bucket = ukfsr::s3_bucket(),
                                       object = "theme_3/input_data/food_chain_tfp.csv")



sector_chart <- tfp |> 
  filter(!Sector %in% c("Food chain", "Wider economy")) |> 
  mutate(Sector = factor(Sector, levels = c("Agriculture", "Manufacturing", "Wholesale", "Retail", "Catering"))) |> 
  ggplot() +
  geom_line(aes(x = Year, y = Productivity, colour = Sector)) +
  scale_colour_manual(values = af_colours(n = 5)) +
  labs(x = NULL, y = "index (2000 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")


save_graphic(sector_chart, "3.4.2d", "food chain sector tfp")


chain_chart <- tfp |> 
  filter(Sector %in% c("Food chain", "Wider economy")) |> 
  # mutate(Sector = factor(Sector, levels = c("Manufacturing", "Wholesale", "Retail", "Catering"))) |> 
  ggplot() +
  geom_line(aes(x = Year, y = Productivity, colour = Sector)) +
  scale_colour_manual(values = af_colours(n = 2)) +
  labs(x = NULL, y = "index (2000 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(chain_chart, "3.4.2c", "food chain tfp")
save_csv(tfp, "3.4.2", "food chain tfp")         
