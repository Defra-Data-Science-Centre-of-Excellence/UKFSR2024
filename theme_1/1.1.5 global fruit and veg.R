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


# Fruit and veg production -----------------------------------------------------
fruit_veg <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_5/input/csv/fruit_and_vegetable_production.csv")%>%
  mutate(area=as.factor(Area))%>%
  mutate(item=as.factor(Item))%>%
  rename(value=Value)%>%
  rename(year=Year)%>%
  select(year,item,value)

fruit_veg_chart <- fruit_veg|>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(fruit_veg_chart, "1.1.5a", "global fruit and vegetable production")
save_csv(fruit_veg, "1.1.5a", "global fruit and vegetable production")
