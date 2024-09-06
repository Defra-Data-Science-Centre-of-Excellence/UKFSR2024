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
library(readxl)

world_data<-map_data("worldLowres")

aquastat <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_2/input/csv/aquastat.csv")

aquastat_world<-aquastat%>%
  filter(Country=="World")

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

direct_drainage_water<-aquastat_country%>%
  filter(Variable=="SDG 6.4.2. Water Stress")%>%
  group_by(Year,Country)%>%
  summarise(Value=sum(Value,na.rm=TRUE))%>%
  filter(Value>90)

direct_drainage_water<-aquastat_regions%>%
  filter(Country=="World")%>%
  filter(Variable=="Total freshwater withdrawal")%>%
  group_by(Year,Country)%>%
  summarise(Value=sum(Value,na.rm=TRUE))

ggplot(data=direct_drainage_water)+
  geom_point(aes(x=Year,y=Value,color=Country))
  
  filter(Country=="World")

percentage_cultivated_area_irrigation<-aquastat_regions%>%
  filter(Variable=="% of the cultivated area equipped for irrigation")%>%
  rename(country=Country)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,country,value)

percentage_cultivated_area_irrigation_temp<-aquastat_country%>%
  filter(Variable=="% of the cultivated area equipped for irrigation")%>%
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

percentage_cultivated_area_irrigation_somalia<-percentage_cultivated_area_irrigation_temp%>%
  mutate(country=if_else(country=="Somalia","Somaliland",country))%>%
  filter(country=="Somaliland")

percentage_cultivated_area_irrigation<-rbind(percentage_cultivated_area_irrigation_somalia)%>%
  filter(year==2021)

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
  mutate(key=if_else(value<25,"no stress",if_else(value<50,"low",if_else(value<75,"medium",if_else(value<100,"high","critical")))))%>%
  mutate(key=if_else(is.na(value),"no data",key))%>%
  mutate(key=ordered(key,levels=c("no data","no stress","low","medium","high","critical")))

world_map_water_stress_chart<-ggplot()+ 
  geom_polygon(data = world_map_water_stress,aes(x=long,y=lat,group=group,fill=key))+
  scale_color_manual(values = af_colours("categorical",n=5)) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(world_map_water_stress_chart, "1.2.2", "water_stress")
save_csv(water_stress, "1.2.2", "water stress.csv")

########

world_map_agricultural_water_withdrawal_percentage<-world_data%>%
  left_join(agricultural_water_withdrawal_percentage,by=c("region"="country"))%>%
  mutate(key=if_else(value<10,"less than 10%",if_else(value<25,"10-25%",if_else(value<50,"25-50%",if_else(value<100,"50-100%",">100%")))))%>%
  mutate(key=if_else(is.na(key),"no data",key))%>%
  mutate(key=ordered(key,levels=c("no data","less than 10%","10-25%","25-50%","50-100%",">100%")))%>%
  filter(year==2021)

agricultural_water_withdrawal_chart<-ggplot()+
  geom_line(data=agricultural_water_withdrawal,aes(x = year, y = value, colour = country)) +
  geom_point(data=agricultural_water_withdrawal,aes(x = year, y = value, colour = country,shape=country),size=4) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  scale_colour_manual(values = c(af_colours("categorical",n=6),"#F46A25")) +
  guides(colour=guide_legend(nrow=4,byrow=TRUE))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "10^9_M3_YEAR")





save_graphic(agricultural_water_withdrawal_chart, "1.2.2", "agricultural_water_withdrawal")
save_csv(agricultural_water_withdrawal, "1.2.2", "agricultural_water_withdrawal")


percentage_cultivated_area_irrigation_chart<-ggplot()+
  geom_line(data=percentage_cultivated_area_irrigation,aes(x = year, y = value, colour = country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  #theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_cultivated_area_irrigation_chart, "1.2.2", "percentage cultivated area irrigation")
save_csv(percentage_cultivated_area_irrigation, "1.2.2", "percentage cultivated area irrigation")

agricultural_water_withdrawal_percentage_chart<-ggplot()+
  geom_polygon(data = world_map_agricultural_water_withdrawal_percentage,aes(x=long,y=lat,group=group,fill=key))+
  scale_color_manual(values = af_colours("categorical",n=5)) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(agricultural_water_withdrawal_percentage_chart, "1.2.2", "agricultural water withdrawal percentage")
save_csv(agricultural_water_withdrawal_percentage, "1.2.2", "agricultural water withdrawal percentage")

water_stress_chart<-ggplot()+
  geom_line(data=water_stress,aes(x = year, y = value, colour = country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(water_stress_chart, "1.2.2", "water stress")
save_csv(water_stress, "1.2.2", "water stress")

water_intake <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_2_2/input/csv/water_intake.csv")%>%
  pivot_longer(cols=2:5,names_to="water_type",values_to="value")%>%
  mutate(water_type=factor(water_type,levels=c("Grey","Blue","Green"),ordered = TRUE))%>%
  mutate(water_type=factor(water_type,levels=c("Grey","Blue","Green"),ordered = TRUE))%>%
  filter(water_type!="Total")

water_intake_chart <- water_intake|>
  ggplot() +
  geom_col(aes(x = fct_reorder(Food_Item, value), y = value, fill = water_type), lwd = 1) +
  coord_flip()+
  #scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,10)) +
  scale_fill_manual(values = c("#3D3D3D","#12436D", "#28A197"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  #guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  labs(x = NULL,
       y = "Water footprint (liters/kg) of product)")
