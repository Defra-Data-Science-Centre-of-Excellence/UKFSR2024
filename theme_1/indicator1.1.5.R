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

meat_eggs <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_1_5/input/csv/meat_eggs_milk_poultry_production.csv")%>%
  mutate(Area=as.factor(Area))%>%
  mutate(Item=as.factor(Item))

#meat<-meat_eggs%>%
#  mutate(Item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Beef & Veal",Item))%>%
#  mutate(Item=if_else(Item%in%c("Meat of chickens, fresh or chilled","Meat of ducks, fresh or chilled","Meat of geese, fresh or chilled","Meat of turkeys, fresh or chilled"),"Poultry",Item))%>%
#  mutate(Item=if_else(Item=="Meat of pig with the bone, fresh or chilled","Pigmeat",Item))%>%
#  mutate(Item=if_else(Item=="Meat of sheep, fresh or chilled","Sheepmeat",Item))%>%
#  filter(!Item%in%c("Meat of goat, fresh or chilled","Hen eggs in shell, fresh"))%>%
#  filter(Unit=="t")%>%
#  group_by(Year,Item,Area)%>%
#  summarise(Value=sum(Value))%>%


meat<-meat_eggs%>%
  filter(Item=="Meat, Total")%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="t")

eggs<-meat_eggs%>%
  filter(Item%in%c("Eggs Primary"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="t")

egg_yield_1<-meat_eggs%>%
  filter(Item%in%c("Eggs Primary"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="100 mg/An")

egg_yield_2010<-meat_eggs%>%
  filter(Item%in%c("Eggs Primary"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="100 mg/An")%>%
  filter(Year==2010)

egg_yield<-egg_yield_1%>%
  left_join(egg_yield_2010,by=c("Area"="Area","Item"="Item"))%>%
  mutate(Value=(Value.x/Value.y)*100)

milk<-meat_eggs%>%
  filter(Item%in%c("Milk, Total"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="t")

milk_yield_1<-meat_eggs%>%
  filter(Item%in%c("Milk, Total"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="100 g/An")

milk_yield_2010<-meat_eggs%>%
  filter(Item%in%c("Milk, Total"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="100 g/An")%>%
  filter(Year==2010)

milk_yield<-milk_yield_1%>%
  left_join(milk_yield_2010,by=c("Area"="Area","Item"="Item"))%>%
  mutate(Value=(Value.x/Value.y)*100)

poultry<-meat_eggs%>%
  filter(Item%in%c("Meat, Poultry"))%>%
  filter(Area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(Unit=="t")
  
world_meat_production_chart <- meat |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat, "1.1.5", "global meat production")

world_egg_production_chart <- eggs |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_egg_production_chart, "1.1.5", "global egg production")
save_csv(eggs, "1.1.5", "global egg production")

world_egg_yield_chart <- egg_yield |>
  ggplot() +
  geom_line(aes(x = Year.x, y = Value, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index 2010=100")

save_graphic(world_egg_yield_chart, "1.1.5", "global egg yield")
save_csv(egg_yield, "1.1.5", "global egg yield")

world_milk_production_chart <- milk |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_milk_production_chart, "1.1.5", "global milk production")
save_csv(milk, "1.1.5", "global milk production")

world_milk_yield_chart <- milk_yield |>
  ggplot() +
  geom_line(aes(x = Year.x, y = Value, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Northern America","Australia and New Zealand","Europe","South America","Asia","Africa")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index 2010=100")

save_graphic(world_milk_yield_chart, "1.1.5", "global milk yield")
save_csv(milk_yield, "1.1.5", "global milk yield")

world_poultry_production_chart <- poultry |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Northern America","Europe","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_poultry_production_chart, "1.1.5", "global poultry production")
save_csv(poultry, "1.1.5", "global poultry production")

meat_type <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_5/input/csv/meat_production.csv")%>%
  filter(Unit=="t")

meat_type_region<-meat_type%>%
  filter(Area%in%c("Africa","Northern America","South America","Asia","Europe","Australia and New Zealand"))

beef_veal_region<-meat_type_region%>%
  filter(Item=="Meat of cattle with the bone, fresh or chilled")%>%
  mutate(Item="Beef and Veal")

world_beef_veal_production_chart <- beef_veal_region |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("South America","Asia","Northern America","Europe","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_beef_veal_production_chart, "1.1.5", "global beef and veal production")
save_csv(beef_veal_region, "1.1.5", "global beef and veal production")

pigmeat_region<-meat_type_region%>%
  filter(Item=="Meat of pig with the bone, fresh or chilled")%>%
  mutate(Item="Pigmeat")

world_pigmeat_production_chart <- pigmeat_region |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_pigmeat_production_chart, "1.1.5", "global pigmeat production")
save_csv(pigmeat_region, "1.1.5", "global pigmeat production")

sheepmeat_region<-meat_type_region%>%
  filter(Item=="Meat of sheep, fresh or chilled")%>%
  mutate(Item="Sheepmeat")

world_sheepmeat_production_chart <- sheepmeat_region |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Africa","Australia and New Zealand","Europe","South America","Northern America")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_sheepmeat_production_chart, "1.1.5", "global sheepmeat production")
save_csv(sheepmeat_region, "1.1.5", "global sheepmeat production")

meat_type_global_poultry<-meat_eggs%>%
  filter(Area=="World")%>%
  filter(Item=="Meat, Poultry")%>%
  filter(Unit=="t")

meat_type_global_1<-meat_type%>%
  filter(Area=="World")%>%
  mutate(Item=if_else(Item=="Meat of sheep, fresh or chilled","Sheepmeat",Item))%>%
  mutate(Item=if_else(Item=="Meat of pig with the bone, fresh or chilled","Pigmeat",Item))%>%
  mutate(Item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Beef and Veal",Item))

meat_type_global<-rbind(meat_type_global_1,meat_type_global_poultry)

world_meat_production_chart <- meat_type_global |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Meat, Poultry","Pigmeat","Beef and Veal","Sheepmeat")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat_type_global, "1.1.5", "global meat production")
