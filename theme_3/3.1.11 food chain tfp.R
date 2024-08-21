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
                                       object = "theme_3/t3_1_11/output/csv/3_1_11_food_chain_tfp.csv")



sector_chart <- tfp |> 
  filter(!Sector %in% c("Food chain", "Wider economy")) |> 
  mutate(Sector = factor(Sector, levels = c("Manufacturing", "Wholesale", "Retail", "Catering"))) |> 
  ggplot() +
  geom_line(aes(x = Year, y = Productivity, colour = Sector)) +
  scale_colour_manual(values = af_colours(n = 4)) +
  labs(x = NULL, y = "index (2000 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website") + 
  theme(axis.ticks.x = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black")) 


save_graphic(sector_chart, "3.1.11", "food chain sector tfp")


chain_chart <- tfp |> 
  filter(Sector %in% c("Food chain", "Wider economy")) |> 
  # mutate(Sector = factor(Sector, levels = c("Manufacturing", "Wholesale", "Retail", "Catering"))) |> 
  ggplot() +
  geom_line(aes(x = Year, y = Productivity, colour = Sector)) +
  scale_colour_manual(values = af_colours(n = 2)) +
  labs(x = NULL, y = "index (2000 = 100)") +
  theme_ukfsr(base_family = "GDS Transport Website") + 
  theme(axis.ticks = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"))

save_graphic(chain_chart, "3.1.11", "food chain tfp")
         