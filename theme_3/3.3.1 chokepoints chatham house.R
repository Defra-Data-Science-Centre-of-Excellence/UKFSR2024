library(dplyr)
library(ukfsr)
library(afcolours)
library(aws.s3)
library(ggplot2)

source(here::here("utils", "load-font.R"))

cp <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/chatham_house_chokepoints_share_of_trade_percent_volume.csv") |> 
  mutate(area = factor(area,
                       levels  = c(
                         "Panama Canal",
                         "Strait of Gibralter",
                         "English Channel",
                         "Turkish Straits",
                         "Suez Canal",
                         "Strait of Bab-al-Mandab",
                         "Strait of Hormuz",
                         "Strait of Malacca"
                       ),
                       labels = c(
                         "Panama\nCanal",
                         "Strait of\nGibraltar",
                         "English\nChannel",
                         "Turkish \nStraits",
                         "Suez\n Canal",
                         "Strait of\nBab\nal-Mandab",
                         "Strait of\nHormuz",
                         "Strait of\nMalacca"
                       )
                      )
                      )


# Cereals ----------------------------------------------------------------------

cereal_chart <- cp |> 
  filter(category2 %in% c("Cereals", "Oilseeds"), !category3 %in% c("Barley", "Other")) |> 
  mutate(category3 = factor(category3, levels = c("Maize", "Wheat", "Rice", "Soybean"))) |> 
  ggplot() +
  geom_col(aes(x = area, y = value, fill = category3), position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = af_colours(n = 4)) + 
  labs(x = NULL, y = NULL) +
  # coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) 

save_graphic(cereal_chart, "3.3.1b", "chokepoints share of trade cereals")

fert_chart <- cp |> 
  filter(category1 == "Fertilizers") |> 
  mutate(category2 = factor(category2, 
                            levels = c("Nitrogenous fertilizers",
                                       "Phosphatic fertilizers",
                                       "Potassic fertilizers",
                                       "Organic fertilizers",
                                       "Mixed fertilizers"), 
                            labels = c("Nitrogenous (N)",
                                       "Phosphatic (P)",
                                       "Potassic (K)",
                                       "Organic",
                                       "Mixed")
                            )) |>
  ggplot() +
  geom_col(aes(x = area, y = value, fill = category2), position = position_dodge()) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = af_colours(n = 5)) + 
  labs(x = NULL, y = NULL) +
  # coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) 


save_graphic(fert_chart, "3.3.1c", "chokepoints share of trade fertiliser")
