library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)
library(sf)

source(here::here("utils", "load-font.R"))


# original data source is 
# https://environment.data.gov.uk/explore/62514eb5-e9d5-4d96-8b73-a40c5b702d43?download=true
water <- aws.s3::s3read_using(FUN = st_read,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/Water_Resource_Availability_and_Abstraction_Reliability_Cycle_2.json")


ukmap <- aws.s3::s3read_using(FUN = st_read,
                              bucket = ukfsr::s3_bucket(),
                              object = "/assets/maps/ukfsr_uk_nuts1/ukfsr_uk_nuts1.geojson")

water <- water |> 
          mutate(camscdsq95 = factor(camscdsq95,
                                     levels = c("Grey", "Green", "Yellow", "Red"),
                                     labels = c("Heavily modified\nwater bodies",
                                                "Water available\nfor licensing",
                                                "Restricted water\navailable for licensing",
                                                "Water not available\nfor licensing")))


 
map <- ggplot(ukmap |> filter(! NUTS118CD %in% c("UKL", "UKM", "UKN"))) +
  geom_sf() +
  geom_sf(data = water, aes(fill = camscdsq95), lwd = 0) +
  scale_fill_manual(values = af_colours(type = "categorical", n = 4)) +
  theme_void() +
  theme(text = element_text(family = "GDS Transport Website", size = 18),
        legend.title = element_blank(),
        legend.position = "inside", 
        legend.position.inside = c(1.1, 0.75), legend.key.spacing = unit(x = 3, units = "mm"))

save_graphic(map, "3.1.3b", "water resource availability")
