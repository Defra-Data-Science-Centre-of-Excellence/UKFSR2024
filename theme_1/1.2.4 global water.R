### Data
library(fastmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(readxl)
library(rworldmap)
library(tmap)
library(sf)
library(ggpattern)

world_data<-map_data("world")

# Agricultural water withdrawal-------------------------------------------------

aquastat <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/input_data/t1_2_4/aquastat.csv")


aquastat_regions<-aquastat%>%
  filter(Country%in%c("World","Northern Africa and Western Asia","Latin America and the Caribbean","Europe and Northern America","Sub-Saharan Africa","Eastern and South-Eastern Asia","Central and Southern Asia"))

aquastat_countries<-unique(aquastat$Country)[1:200]

aquastat_country<-aquastat%>%
  filter(Country%in%aquastat_countries)

agricultural_water_withdrawal<-aquastat_regions%>%
  filter(Variable=="Agricultural water withdrawal")%>%
  rename(country=Country)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,country,value)

agricultural_water_withdrawal_chart<-ggplot()+
  geom_line(data=agricultural_water_withdrawal,aes(x = year, y = value, colour = country)) +
  geom_point(data=agricultural_water_withdrawal,aes(x = year, y = value, colour = country,fill=country,shape=country),size=4) +
  scale_colour_manual(values = c(af_colours("categorical",n=6),"#F46A25")) +
  scale_fill_manual(values = c(af_colours("categorical",n=6),"#F46A25")) +
  scale_shape_manual(values=c(25,NA,NA,NA,NA,NA,25))+
  guides(colour=guide_legend(nrow=4,byrow=TRUE))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Billion litres per year")

save_graphic(agricultural_water_withdrawal_chart, "1.2.4a", "agricultural water withdrawal")
save_csv(agricultural_water_withdrawal, "1.2.4a", "agricultural water withdrawal")

# Percentage agriculture withdrawal --------------------------------------------

agricultural_water_withdrawal_percentage_temp<-aquastat_country%>%
  filter(Variable=="Agricultural water withdrawal as % of total renewable water resources")%>%
  rename(country=Country)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  mutate(country=if_else(country=="United States of America","USA",country))%>%
  mutate(country=if_else(country=="Syrian Arab Republic","Syria",country))%>%
  mutate(country=if_else(country=="Netherlands (Kingdom of the)","Netherlands",country))%>%
  mutate(country=if_else(country=="Viet Nam","Vietnam",country))%>%
  mutate(country=if_else(country=="Brunei Darussalam","Brunei",country))%>%
  mutate(country=if_else(country=="Venezuela (Bolivarian Republic of)","Venezuela",country))%>%
  mutate(country=if_else(country=="Türkiye","Turkey",country))%>%
  mutate(country=if_else(country=="Antigua and Barbuda","Antigua",country))%>%
  mutate(country=if_else(country=="Republic of Korea","South Korea",country))%>%
  mutate(country=if_else(country=="Democratic People's Republic of Korea","North Korea",country))%>%
  mutate(country=if_else(country=="Saint Kitts and Nevis","Saint Kitts",country))%>%
  mutate(country=if_else(country=="Republic of Moldova","Moldova",country))%>%
  mutate(country=if_else(country=="Russian Federation","Russia",country))%>%
  mutate(country=if_else(country=="Saint Vincent and the Grenadines","Grenadines",country))%>%
  mutate(country=if_else(country=="Côte d'Ivoire","Ivory Coast",country))%>%
  mutate(country=if_else(country=="Trinidad and Tobago","Trinidad",country))%>%
  mutate(country=if_else(country=="Czechia","Czech Republic",country))%>%
  mutate(country=if_else(country=="Iran (Islamic Republic of)","Iran",country))%>%
  mutate(country=if_else(country=="Bolivia (Plurinational State of)","Bolivia",country))%>%
  mutate(country=if_else(country=="Cabo Verde","Cape Verde",country))%>%
  mutate(country=if_else(country=="Lao People's Democratic Republic","Laos",country))%>%
  mutate(country=if_else(country=="United Republic of Tanzania","Tanzania",country))%>%
  mutate(country=if_else(country=="United Kingdom of Great Britain and Northern Ireland","UK",country))%>%
  mutate(country=if_else(country=="Congo","Republic of Congo",country))%>%
  mutate(country=if_else(country=="Eswatini","Swaziland",country))%>%
  select(year,country,value)

agricultural_water_withdrawal_percentage_somalia<-agricultural_water_withdrawal_percentage_temp%>%
  mutate(country=if_else(country=="Somalia","Somaliland",country))%>%
  filter(country=="Somaliland")

agricultural_water_withdrawal_percentage<-rbind(agricultural_water_withdrawal_percentage_temp,agricultural_water_withdrawal_percentage_somalia)%>%
  filter(year==2021)

world_map_agricultural_water_withdrawal_percentage<-world_data%>%
  left_join(agricultural_water_withdrawal_percentage,by=c("region"="country"))%>%
  mutate(key=if_else(value<10,"Less than 10%",if_else(value<25,"10-25%",if_else(value<50,"25-50%",if_else(value<100,"50-100%",">100%")))))%>%
  mutate(key=if_else(is.na(key),"No data",key))%>%
  mutate(key=ordered(key,levels=c("No data","Less than 10%","10-25%","25-50%","50-100%",">100%")))%>%
  filter(year==2021)

agricultural_water_withdrawal_percentage_chart<-
  ggplot()+
  geom_polygon(data = world_data |> filter(region == "Greenland", is.na(subregion)), aes(x = long, y = lat), fill = "grey90") +
  geom_polygon(data = world_map_agricultural_water_withdrawal_percentage,aes(x=long,y=lat,group=group,fill=key))+
  scale_fill_manual(values = c("#28A197","#12436D","#A285D1","#801650","#F46A25")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_blank())

save_graphic(agricultural_water_withdrawal_percentage_chart, "1.2.4b", "agricultural water withdrawal percentage")
save_csv(agricultural_water_withdrawal_percentage, "1.2.4b", "agricultural water withdrawal percentage")




# Area equipped for irrigation: actually irrigated-------------------------------------------------

area_equipped_for_irrigation_actually_irrigated<-aquastat_regions%>%
  filter(Variable=="Area equipped for irrigation: actually irrigated")%>%
  rename(country=Country)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,country,value)

area_equipped_for_irrigation_actually_irrigated_chart<-ggplot()+
  geom_line(data=area_equipped_for_irrigation_actually_irrigated,aes(x = year, y = value/1e3, colour = country)) +
  geom_point(data=area_equipped_for_irrigation_actually_irrigated,aes(x = year, y = value/1e3, colour = country,fill=country,shape=country),size=4) +
  scale_colour_manual(values = c(af_colours("categorical",n=6),"#F46A25")) +
  scale_fill_manual(values = c(af_colours("categorical",n=6),"#F46A25")) +
  scale_x_continuous(limits=c(2000,2020))+
  scale_shape_manual(values=c(25,NA,NA,NA,NA,NA,25))+
  guides(colour=guide_legend(nrow=4,byrow=TRUE))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(plot.margin=unit(c(0.4,1,0.4,1),"cm"))+
  labs(x = NULL,
       y = "")

save_graphic(area_equipped_for_irrigation_actually_irrigated_chart, "1.2.4c", "area equipped for irrigation: actually irrigated")
save_csv(area_equipped_for_irrigation_actually_irrigated, "1.2.4c", "area equipped for irrigation: actually irrigated")


# Water stress -----------------------------------------------------------------

water_stress<-aquastat_country%>%
  filter(Variable=="SDG 6.4.2. Water Stress")%>%
  rename(country=Country)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,country,value)

water_stress<-water_stress%>%
  filter(year==2021)%>%
  mutate(country=if_else(country=="United States of America","USA",country))%>%
  mutate(country=if_else(country=="Syrian Arab Republic","Syria",country))%>%
  mutate(country=if_else(country=="Netherlands (Kingdom of the)","Netherlands",country))%>%
  mutate(country=if_else(country=="Viet Nam","Vietnam",country))%>%
  mutate(country=if_else(country=="Brunei Darussalam","Brunei",country))%>%
  mutate(country=if_else(country=="Venezuela (Bolivarian Republic of)","Venezuela",country))%>%
  mutate(country=if_else(country=="Türkiye","Turkey",country))%>%
  mutate(country=if_else(country=="Antigua and Barbuda","Antigua",country))%>%
  mutate(country=if_else(country=="Republic of Korea","South Korea",country))%>%
  mutate(country=if_else(country=="Democratic People's Republic of Korea","North Korea",country))%>%
  mutate(country=if_else(country=="Saint Kitts and Nevis","Saint Kitts",country))%>%
  mutate(country=if_else(country=="Republic of Moldova","Moldova",country))%>%
  mutate(country=if_else(country=="Russian Federation","Russia",country))%>%
  mutate(country=if_else(country=="Saint Vincent and the Grenadines","Grenadines",country))%>%
  mutate(country=if_else(country=="Côte d'Ivoire","Ivory Coast",country))%>%
  mutate(country=if_else(country=="Trinidad and Tobago","Trinidad",country))%>%
  mutate(country=if_else(country=="Czechia","Czech Republic",country))%>%
  mutate(country=if_else(country=="Iran (Islamic Republic of)","Iran",country))%>%
  mutate(country=if_else(country=="Bolivia (Plurinational State of)","Bolivia",country))%>%
  mutate(country=if_else(country=="Cabo Verde","Cape Verde",country))%>%
  mutate(country=if_else(country=="Lao People's Democratic Republic","Laos",country))%>%
  mutate(country=if_else(country=="United Republic of Tanzania","Tanzania",country))%>%
  mutate(country=if_else(country=="United Kingdom of Great Britain and Northern Ireland","UK",country))%>%
  mutate(country=if_else(country=="Congo","Republic of Congo",country))%>%
  mutate(country=if_else(country=="Eswatini","Swaziland",country))

world_map_water_stress<-world_data%>%
  left_join(water_stress,by=c("region"="country"))%>%
  mutate(key=if_else(value<25,"No stress",if_else(value<50,"Low (<50%)",if_else(value<75,"Medium (50-75%)",if_else(value<100,"High (75-100%)","Critical (>100%)")))))%>%
  #mutate(key=if_else(is.na(value),"no data",key))%>%
  filter(!is.na(key))%>%
  mutate(key=ordered(key,levels=c("No stress","Low (<50%)","Medium (50-75%)","High (75-100%)","Critical (>100%)")))


world_map_water_stress_chart<-
  ggplot()+
  geom_polygon(data = world_data |> filter(region == "Greenland", is.na(subregion)), aes(x = long, y = lat), fill = "grey90") +
  geom_polygon(data = world_map_water_stress |> filter(region != "Antarctica"),aes(x=long,y=lat,group=group,fill=key))+
  scale_fill_manual(values = c("#28A197","#12436D","#A285D1","#801650","#F46A25")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_blank())

save_graphic(world_map_water_stress_chart, "1.2.4d", "water stress")
save_csv(water_stress, "1.2.4d", "water stress")


