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
save_csv(world_meat_production_chart, "1.1.5", "global meat production")

world_egg_production_chart <- eggs |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Area,linetype=Area), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(cereal_yield_chart, "1.1.5", "global egg production")
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