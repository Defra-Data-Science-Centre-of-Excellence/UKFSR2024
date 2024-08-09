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
  mutate(area=as.factor(Area))%>%
  mutate(item=as.factor(Item))%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(unit=Unit)

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
  filter(item=="Meat, Total")%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="t")%>%
  select(year,item,area,value,unit)

eggs<-meat_eggs%>%
  filter(item%in%c("Eggs Primary"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="t")%>%
  select(year,item,area,value,unit)

egg_yield_1<-meat_eggs%>%
  filter(item%in%c("Eggs Primary"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="100 mg/An")%>%
  select(year,item,area,value,unit)

egg_yield_2010<-meat_eggs%>%
  filter(item%in%c("Eggs Primary"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="100 mg/An")%>%
  filter(year==2010)%>%
  select(year,item,area,value,unit)

egg_yield<-egg_yield_1%>%
  left_join(egg_yield_2010,by=c("area"="area","item"="item"))%>%
  mutate(value=(value.x/value.y)*100)%>%
  rename(year="year.x")%>%
  rename(unit="unit.x")%>%
  select(year,item,area,value,unit)

milk<-meat_eggs%>%
  filter(item%in%c("Milk, Total"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="t")%>%
  select(year,item,area,value,unit)

milk_yield_1<-meat_eggs%>%
  filter(item%in%c("Milk, Total"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="100 g/An")%>%
  select(year,item,area,value,unit)

milk_yield_2010<-meat_eggs%>%
  filter(item%in%c("Milk, Total"))%>%
  filter(area%in%c("Africa","Asia","Australia and New Zealand","Europe","Northern America","South America"))%>%
  filter(unit=="100 g/An")%>%
  filter(year==2010)%>%
  select(year,item,area,value,unit)

milk_yield<-milk_yield_1%>%
  left_join(milk_yield_2010,by=c("area"="area","item"="item"))%>%
  mutate(value=(value.x/value.y)*100)%>%
  rename(year="year.x")%>%
  rename(unit="unit.x")%>%
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

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat, "1.1.5", "global meat production")

world_egg_production_chart <- eggs |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_egg_production_chart, "1.1.5", "global egg production")
save_csv(eggs, "1.1.5", "global egg production")

world_egg_yield_chart <- egg_yield |>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index 2010=100")

save_graphic(world_egg_yield_chart, "1.1.5", "global egg yield")
save_csv(egg_yield, "1.1.5", "global egg yield")

world_milk_production_chart <- milk |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Asia","Europe","Northern America","South America","Africa","Australia and New Zealand")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_milk_production_chart, "1.1.5", "global milk production")
save_csv(milk, "1.1.5", "global milk production")

world_milk_yield_chart <- milk_yield |>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1965,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("Northern America","Australia and New Zealand","Europe","South America","Asia","Africa")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index 2010=100")

save_graphic(world_milk_yield_chart, "1.1.5", "global milk yield")
save_csv(milk_yield, "1.1.5", "global milk yield")

world_poultry_production_chart <- poultry |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
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
  filter(area=="World")%>%
  filter(item=="Meat, Poultry")%>%
  filter(unit=="t")%>%
  select(year,item,value,unit)

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

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat_type_global, "1.1.5", "global meat production")

greenhouse_gas_emission_meat <- aws.s3::s3read_using(FUN = read_csv,
                                                        bucket = ukfsr::s3_bucket(),
                                                        object = "theme_1/t1_1_5/input/csv/greenhousegasemissionmeat.csv")%>%
  pivot_longer(cols=2:5,names_to="stat",values_to = "value")

greenhouse_gas_emission_meat_chart <- greenhouse_gas_emission_meat|>
  ggplot() +
  geom_boxplot(aes(Type,value), lwd = 1) +
  #scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("duo"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kg CO2-eq/kg produce")

save_graphic(greenhouse_gas_emission_meat_chart, "1.1.5", "greenhouse_gas_emission_meat")
save_csv(greenhouse_gas_emission_meat, "1.1.5", "greenhouse_gas_emission_meat")
