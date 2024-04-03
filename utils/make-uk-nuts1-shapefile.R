library(sf)
library(here)
library(dplyr)
library(readr)

# LAD/NUTS lookup codes from
# https://geoportal.statistics.gov.uk/datasets/ons::lad-december-2018-to-nuts3-to-nuts2-to-nuts1-january-2018-lookup-in-the-uk/about
lookup <- read_csv("Local_Authority_District_(December_2018)_to_NUTS3_to_NUTS2_to_NUTS1_(January_2018)_Lookup_in_United_Kingdom.csv")

# Boundaries taken from
# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2018-boundaries-uk-buc-1/about
base_map <- st_read("LAD_December_2018_Boundaries_UK_BUC.shp")


uk_map <- base_map |> 
  left_join(lookup, by = c("lad18cd" = "LAD18CD")) |> 
  filter(!(lad18cd %in% c("S12000027", "S12000023"))) |> 
  group_by(NUTS118NM, NUTS118CD) |> 
  dplyr::summarise(geometry = st_union(geometry), .groups = "drop")

# st_write(uk_map, "ukfsr_uk_nuts1.shp")
