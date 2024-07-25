### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

#FAOSTAT

source(here("utils", "load-font.R"))

fruit_veg <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_1_6/input/csv/fruit_and_vegetable_production.csv")%>%
  mutate(Area=as.factor(Area))%>%
  mutate(Item=as.factor(Item))

fruit_veg_chart <- fruit_veg|>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(fruit_veg_chart, "1.1.6", "global fruit and vegetable production")
save_csv(fruit_veg, "1.1.6", "global fruit and vegetable production")

bananas <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_6/input/csv/banana_production.csv")%>%
  mutate(Area=as.factor(Area))%>%
  mutate(Item=as.factor(Item))

bananas_chart <- bananas|>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("duo"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(bananas_chart, "1.1.6", "global banana production")
save_csv(bananas, "1.1.6", "global banana production")
