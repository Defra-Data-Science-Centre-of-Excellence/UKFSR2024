library(janitor)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(afcolours)
library(ukfsr)
library(readODS)

UAA_data <- aws.s3::s3read_using(FUN = read_ods,
                                 bucket = "s3-ranch-054",
                                 object = "theme_2/t2_4_1/input/structure-june-uktimeseries-14dec23.ods",
                                 sheet = "UK_timeseries", 
                                 col_names = TRUE,
                                 skip = 1) |> 
  clean_names()

current_year <- "x2023"

UAA_data <- UAA_data |> 
  rename(land_use = "table_1_land_use_a_hectares",
         x2009 = "x2009_b",
         x2022 = "x2022_f") |> 
  select(land_use, x1984:glue("{current_year}")) |> 
  pivot_longer(cols = x1984:glue("{current_year}"),names_to = "year", values_to = "value") |> 
  mutate(year = substring(year,2,5)) |> 
  mutate(year = as.numeric(year)) |> 
  mutate(value = as.numeric(value)) |> 
  filter(land_use %in% c("Common rough grazing",
                         "Total crops",
                         "Uncropped arable land(d)",
                         "Temporary grass under 5 years old",
                         "Total permanent grassland")) |> 
  mutate(land_use = factor(land_use,
                           levels = c("Total permanent grassland",
                                      "Temporary grass under 5 years old", 
                                      "Total crops",
                                      "Uncropped arable land(d)",
                                      "Common rough grazing"),
                           labels = c("Permanent grassland",
                                      "Temporary grass <5 years old", 
                                      "Crops",
                                      "Uncropped arable land",
                                      "Common rough grazing")))


chart <- UAA_data |> 
  filter(year>=2014) |>
  mutate(land_use = forcats::fct_rev(land_use)) |>
ggplot(aes(x = year, y = value/1e6, fill = land_use)) +
  geom_bar(position = "stack",
           stat = "identity",
           width = 0.8) +
  geom_text(aes(label = round(value/1e6, 1)), 
            position = position_stack(vjust = 0.5),
            colour = "white",
            size = 7,
            family = "GDS Transport Website") +
  scale_fill_manual(values=rev(af_colours())) +
  labs(y = "Million Hectares", x = NULL) +
  guides(fill = guide_legend(nrow=2, byrow=TRUE, reverse = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website") 

save_graphic(chart, "fsi.4.1", "uaa fsi")
save_csv(UAA_data, "fsi.4.1", "uaa fsi")
