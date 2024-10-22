#devtools::install_github("FoodchainStats/ukfsr")


library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

# Spray irrigation and storage -------------------------------------------------

FSR_3_1_3a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/Water irrigation storage.csv")


FSR_3_1_3a <- FSR_3_1_3a %>%
  pivot_longer(cols = c( "spray irrigation - storage ('000m3)", "spray irrigation ('000m3)"), names_to = "Type", values_to = "Volume")

FSR_3_1_3a <- FSR_3_1_3a %>%
  arrange(Region_Code) %>%
  mutate(Region = factor(Region, levels = unique(Region)))

# Plot the bar chart
FSR_3_1_3a_plot <- ggplot(FSR_3_1_3a, aes(x = Region, y = Volume / 1e3, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = afcolours::af_colours("duo"), labels = c("Spray irrigation - storage", "Spray irrigation")) +
  labs(y = expression(Volume~(millions~m^3)),
       x = NULL,
       fill = "Type") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5)) +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) 

FSR_3_1_3a_plot

save_graphic(FSR_3_1_3a_plot, '3.2.1a', 'spray irrigation and storage') + 
  save_csv(FSR_3_1_3a, '3.2.1a', 'spray irrigation and storage')


# Map (Water Resource Availability) --------------------------------------------
  
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

# Experimental simplification since the original svg came out at 63mb. Gets file
# size down to 19mb. We combine polygons of the same type into a single
# multipolygon

sf_use_s2(FALSE)
watersimple <- water |> group_by(camscdsq95) |> summarise(geometry = st_union(geometry))

# Further simplification gets us down to 1.7mb. This just reduces the complexity
# of the polygons

xsimple <- st_simplify(watersimple, dTolerance = 0.001)

xmap <- ggplot(ukmap |> filter(! NUTS118CD %in% c("UKL", "UKM", "UKN"))) +
  geom_sf() +
  geom_sf(data = xsimple, aes(fill = camscdsq95), lwd = 0) +
  scale_fill_manual(values = af_colours(type = "categorical", n = 4)) +
  theme_void() +
  theme(text = element_text(family = "GDS Transport Website", size = 26),
        legend.title = element_blank(),
        legend.position = "inside", 
        legend.position.inside = c(1.1, 0.75), legend.key.spacing = unit(x = 3, units = "mm"))


save_graphic(xmap, "3.2.1c", "water resource availability super simplified")
