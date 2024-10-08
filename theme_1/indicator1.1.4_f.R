### Data
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)


#FAOSTAT

source(here("utils", "load-font.R"))

meat_eggs <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_4/input/csv/meat_eggs_milk_poultry_production.csv")%>%
  mutate(area=as.factor(Area))%>%
  mutate(item=as.factor(Item))%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(unit=Unit)

meat<-meat_eggs%>%
  filter(item=="Meat, Total")%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="t")%>%
  select(year,item,area,value,unit)

poultry<-meat_eggs%>%
  filter(item%in%c("Meat, Poultry"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="t")%>%
  select(year,item,area,value,unit)

world_meat_production_chart <- meat |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.4", "global meat production")
save_csv(meat, "1.1.4", "global meat production")


meat_type_global_poultry<-meat_eggs%>%
  filter(area=="World")%>%
  filter(item=="Meat, Poultry")%>%
  filter(unit=="t")%>%
  select(year,item,value,unit)

meat_type <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_4/input/csv/meat_production.csv")%>%
  filter(Unit=="t")

meat_type_global_1<-meat_type%>%
  filter(Area=="World")%>%
  rename(area=Area)%>%
  rename(unit=Unit)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  mutate(item=if_else(Item=="Meat of sheep, fresh or chilled","Sheepmeat",Item))%>%
  mutate(item=if_else(Item=="Meat of pig with the bone, fresh or chilled","Pigmeat",Item))%>%
  mutate(item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Beef and Veal",Item))%>%
  select(year,item,value,unit)

meat_type_global<-rbind(meat_type_global_1,meat_type_global_poultry)

world_meat_production_chart <- meat_type_global |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Meat, Poultry","Pigmeat","Beef and Veal","Sheepmeat")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.4", "global meat production")
save_csv(meat_type_global, "1.1.4", "global meat production")
