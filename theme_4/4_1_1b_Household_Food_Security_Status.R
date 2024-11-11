library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
#library(aws.ec2.metadata)
library(stringr)
library(data.table)
library(ggrepel)
library(scales)
library(zoo)
library(plyr)
library(grid)
library(ggpp)
library(ggrepel)
library(png)
library(viridis)
library(showtext)
library(svglite)
library(here)
library(sf)
library(janitor)

source(here("utils", "load-font.R"))

t4_1_1b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/4_1_4b_household_food_security_region_FYE_2023.csv")%>% clean_names()

#max(FSR_4_1_4a$food_secure) >- highest security is 13
#min(FSR_4_1_4a$food_secure) >- lowest food security is 8

nuts1 <- aws.s3::s3read_using(FUN = sf::st_read,
                              bucket = "s3-ranch-054",
                              object = "assets/maps/ukfsr_uk_nuts1/ukfsr_uk_nuts1.geojson")%>% clean_names()


#map with three bands
mapdata <- nuts1 %>% 
  left_join(t4_1_1b) %>% 
  mutate(secure_bands = case_when(food_secure > 90 ~ "91-92",
                                  food_secure <=90 & food_secure > 88 ~ "89-90",
                                  food_secure <= 88 ~ "87-88")) %>%
  mutate(secure_bands = factor(secure_bands, levels = c("91-92","89-90","87-88"), ordered = TRUE))

af_sequential_colours <- c(
  "#12436D", # Dark blue
  "#2073BC", # Mid blue
  "#6BACE6" # Light blue
)

t4_1_1b_plot <- ggplot(mapdata) +
  geom_sf(aes(fill = secure_bands),  lwd = 0.1) +
  geom_sf_text(aes(label = stringr::str_wrap(region, 5)),
               nudge_x = c(0,0,10000,0,20000,0,-20000,60000,rep(0, 4)),
               nudge_y = c(rep(0, 3),-20000,0,0,-20000,-40000,-10000,rep(0, 3)),
               size = 5, colour = "white", fontface = "bold") +
  scale_fill_manual(values = af_sequential_colours,
                    name = "% of \nhouseholds \nthat are food\nsecure") +
  theme_ukfsr()+
  theme_void() + 
  theme(text = element_text(size = 20)) +
  theme(legend.text=element_text(size=18))  +
  theme(legend.position = "inside", legend.position.inside = c(0.8,0.75))

t4_1_1b_plot

save_graphic(t4_1_1b_plot, "4.1.1b", "household food security region FYE_2023")

save_csv(t4_1_1b, "4.1.1b", "household food security region FYE 2023")


