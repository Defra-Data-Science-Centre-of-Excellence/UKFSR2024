### Data
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(patchwork)
library(here)


#FAOSTAT

source(here("utils", "load-font.R"))

food_supply_by_region <- aws.s3::s3read_using(FUN = read_csv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/t1_1_4/input/csv/RegionDemand.csv")

food_supply_asia<-food_supply_by_region%>%filter(Region=="Asia")#%>%filter(Area!="Central Asia")
food_supply_africa<-food_supply_by_region%>%filter(Region=="Africa")#%>%filter(Area!="Middle Africa")
food_supply_americas<-food_supply_by_region%>%filter(Region=="Americas")
food_supply_europe<-food_supply_by_region%>%filter(Region=="Europe")
food_supply_oceania<-food_supply_by_region%>%filter(Region=="Oceania")%>%mutate(Area=if_else(Area=="Australia and New Zealand","Australia and\n New Zealand",Area))
food_supply_world<-food_supply_by_region%>%filter(Region=="World")



p1<-ggplot(food_supply_asia,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")


p2<-ggplot(food_supply_africa,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")


p3<-ggplot(food_supply_americas,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")


p4<-ggplot(food_supply_europe,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")


p5<-ggplot(food_supply_oceania,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")


p6<-ggplot(food_supply_world,aes(x=Year,y=Value,color=Area))+
  geom_line(linewidth=2)+
  facet_wrap(vars(Region))+
  scale_x_continuous(breaks=seq(2010,2021,5))+
  scale_color_manual(values=af_colours("categorical"))+
  theme_ukfsr()+
  theme(legend.position = "right")

p_out<-(p1+p2)/(p3+p4)/(p5+p6)

#save_graphic(p_out, "1.1.4", "global food supply by region")

cereal_production_yield <- aws.s3::s3read_using(FUN = read_csv,
                                              bucket = ukfsr::s3_bucket(),
                                              object = "theme_1/t1_1_4/input/csv/cereal_yield_production.csv")

### Processing

#  cereal_production_yield<-cereal_production_yield%>%
#    mutate(Area=if_else(Area%in%c("Northern Africa","Western Asia","Central Asia"),"North Africa, Central and Western Asia",Area))%>%
#    mutate(Area=if_else(Area%in%c("Middle Africa","Western Africa","Eastern Africa","Southern Africa"),"Sub-Saharan Africa",Area))%>%
#    mutate(Area=if_else(Area%in%c("Central America","Caribbean","South America"),"Latin America and Caribbean",Area))%>%
#    mutate(Area=if_else(Area%in%c("Polynesia","Micronesia","Melanesia"),"Oceania (exec Australia and New Zealand)",Area))%>%
#    mutate(Area=if_else(Area%in%c("Eastern Asia","Southern Asia","South-eastern Asia"),"South, East and South East Asia",Area))%>%
#    mutate(Area=if_else(Area%in%c("Northern America"),"North America",Area))%>%
#    group_by(`Area`,`Element`,`Year`)%>%
#    rename(area=Area)%>%
#    rename(year=Year)%>%
#    summarise(value=sum(Value))
# 
# cereal_production_yield<-cereal_production_yield%>%
#   mutate(Area=if_else(Area%in%c("Northern Africa","Middle Africa","Western Africa","Eastern Africa","Southern Africa"),"Sub-Saharan Africa",Area))%>%
#   mutate(Area=if_else(Area%in%c("Central America","Caribbean","South America"),"Latin America and Caribbean",Area))%>%
#   mutate(Area=if_else(Area%in%c("Polynesia","Micronesia","Melanesia"),"Oceania (exec Australia and New Zealand)",Area))%>%
#   mutate(Area=if_else(Area%in%c("Western Asia","Central Asia","Eastern Asia","Southern Asia","South-eastern Asia"),"Asia",Area))%>%
#   mutate(Area=if_else(Area%in%c("Northern America"),"North America",Area))

cereal_production<-cereal_production_yield%>%
  filter(Element=="Production")%>%
  filter(Area%in%c("Africa","Asia","Europe","Northern America","South America"))%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(item=Item)%>%
  select(year,area,item,value)

cereal_yield<-cereal_production_yield%>%
  filter(Element=="Yield")%>%
  #filter(Area%in%c("Africa","Asia","Europe","Northern America","South America","World"))%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(item=Item)%>%
  select(year,area,item,value)


cereal_production_chart <- cereal_production |>
  ggplot() +
  facet_wrap(~item)+
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
  geom_point(aes(x=year, y=value/1E6,colour=area,shape=area))+
  scale_x_continuous(limits = c(1970,2022),breaks =seq(1970,2022,10)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(cereal_production_chart, "1.1.4", "global cereal production")
save_csv(cereal_production, "1.1.4", "global cereal production")

cereal_yield_chart <- cereal_yield |>
  #filter(area!="Australia and New Zealand") |>
  ggplot() +
  facet_wrap(~item)+
  geom_line(aes(x = year, y = value/1000, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1970,2022),breaks =seq(1970,2022,10)) +
  #scale_y_continuous(limits = c(0,9),breaks =seq(0,8,2)) +
  scale_colour_manual(values = af_colours("categorical",n=6),limits=c("Europe","Asia","Africa","South America","Northern America","World")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "tonnes per hectare")

save_graphic(cereal_yield_chart, "1.1.4", "global cereal yields")
save_csv(cereal_yield, "1.1.4", "global cereal yields")

greenhouse_gas_emission_cereals <- aws.s3::s3read_using(FUN = read_csv,
                                                              bucket = ukfsr::s3_bucket(),
                                                              object = "theme_1/t1_1_4/input/csv/greenhousegasemissioncereals.csv")%>%
  pivot_longer(cols=2:5,names_to="stat",values_to = "value")

greenhouse_gas_emission_cereals_chart <- greenhouse_gas_emission_cereals|>
  ggplot() +
  geom_boxplot(aes(Type,value), lwd = 1) +
  #scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("duo"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kg CO2-eq/kg produce")

save_graphic(greenhouse_gas_emission_cereals_chart, "1.1.4", "greenhouse_gas_emission_cereals")
save_csv(greenhouse_gas_emission_cereals, "1.1.4", "greenhouse_gas_emission_cereals")

