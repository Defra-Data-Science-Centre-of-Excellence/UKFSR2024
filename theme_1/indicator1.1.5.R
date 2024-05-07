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
                                    object = "theme_1/t1_1_5/input/csv/meat_eggs_milk_poultry_production.csv")

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
  filter(Area!="World")%>%
  filter(Unit=="t")

eggs<-meat_eggs%>%
  filter(Item%in%c("Eggs Primary"))%>%
  filter(Area!="World")%>%
  filter(Unit=="t")

milk<-meat_eggs%>%
  filter(Item%in%c("Milk, Total"))%>%
  filter(Area!="World")%>%
  filter(Unit=="t")
  
world_meat_production_chart <- meat |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat, "1.1.5", "global meat production")

world_egg_production_chart <- eggs |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_egg_production_chart, "1.1.5", "global egg production")
save_csv(eggs, "1.1.5", "global egg production")

world_milk_production_chart <- milk |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_milk_production_chart, "1.1.5", "global milk production")
save_csv(milk, "1.1.5", "global milk production")

meat_type <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_5/input/csv/meat_production.csv")

meat_type_region<-meat_type%>%
  filter(Area%in%c("Africa","Northern America","South America","Asia","Europe","Australia and New Zealand"))

beef_veal_region<-meat_type_region%>%
  filter(Item=="Meat of cattle with the bone, fresh or chilled")%>%
  mutate(Item="Beef and Veal")

world_beef_veal_production_chart <- beef_veal_region |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
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
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
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
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_sheepmeat_production_chart, "1.1.5", "global sheepmeat production")
save_csv(sheepmeat_region, "1.1.5", "global sheepmeat production")

meat_type_global_poultry<-meat_eggs%>%
  filter(Area=="World")%>%
  filter(Item=="Meat, Poultry")

meat_type_global_1<-meat_type%>%
  filter(Area=="World")%>%
  mutate(Item=if_else(Item=="Meat of sheep, fresh or chilled","Sheepmeat",Item))%>%
  mutate(Item=if_else(Item=="Meat of pig with the bone, fresh or chilled","Pigmeat",Item))%>%
  mutate(Item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Beef and Veal",Item))

meat_type_global<-rbind(meat_type_global_1,meat_type_global_poultry)

world_meat_production_chart <- meat_type_global |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(world_meat_production_chart, "1.1.5", "global meat production")
save_csv(meat_type_global, "1.1.5", "global meat production")