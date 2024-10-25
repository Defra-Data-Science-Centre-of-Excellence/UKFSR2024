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

source(here("utils", "load-font.R"))

# Agricultural land use --------------------------------------------------------

agricultural_land_use <- aws.s3::s3read_using(FUN = read_csv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/input_data/t1_2_2/agricultural_land_use.csv")%>%
  rename(item=Item)%>%
  rename(value=Value)%>%
  rename(area=Area)%>%
  rename(year=Year)



agricultural_land_use_in<-agricultural_land_use%>%
  filter(item%in%c("Permanent meadows and pastures","Cropland","Agricultural land","Forest land"))%>%
  mutate(area=case_when(area=="Northern America"~"North America and Europe",area=="Europe"~"North America and Europe",area=="South America"~"South America and South East Asia",area=="South-eastern Asia"~"South America and South East Asia",TRUE~area))%>%
  select(year,area,item,value)%>%
  filter(year>1989)

agricultural_land_use_1990_1<-agricultural_land_use_in%>%
  filter(item%in%c("Forest land"))%>%
  filter(year==1990)%>%
  mutate(new_item=paste0(item," ",area))%>%
  group_by(year,item,new_item)%>%
  summarise(value=sum(value,na.rm = NA))

agricultural_land_use_1990_2<-agricultural_land_use_in%>%
  filter(item%in%c("Permanent meadows and pastures","Cropland","Agricultural land"))%>%
  filter(area=="World")%>%
  filter(year==1990)%>%
  mutate(new_item=item)%>%
  select(-area)

agricultural_land_use_1990<-rbind(agricultural_land_use_1990_1,agricultural_land_use_1990_2)

agricultural_land_use_1<-agricultural_land_use_in%>%
  filter(item%in%c("Forest land"))%>%
  mutate(new_item=paste0(item," ",area))%>%
  group_by(year,new_item)%>%
  summarise(value=sum(value,na.rm = TRUE))%>%
  left_join(agricultural_land_use_1990,by=c("new_item"="new_item"))%>%
  mutate(index=(value.x/value.y)*100)%>%
  select(year.x,new_item,index)
  
agricultural_land_use_2<-agricultural_land_use_in%>%
  filter(item%in%c("Permanent meadows and pastures","Cropland","Agricultural land"))%>%
  filter(area=="World")%>%  
  left_join(agricultural_land_use_1990,by=c("item"="item"))%>%
  mutate(index=(value.x/value.y)*100)%>%
  mutate(new_item=paste0(item))%>%
  select(year.x,new_item,index)

agricultural_land_use<-rbind(agricultural_land_use_1,agricultural_land_use_2)%>%
  mutate(new_item = factor(new_item,
                           levels = c("Agricultural land", "Cropland", "Permanent meadows and pastures", "Forest land World" , "Forest land North America and Europe", "Forest land South America and South East Asia"),
                           labels = c("Agricultural land", "Cropland", "Permanent meadows and pastures", "Global Forest land" , "Forest land, North America and Europe", "Forest land, South America and South East Asia")))

  

agricultural_land_use_chart <- agricultural_land_use|>
  ggplot() +
  geom_line(aes(x = year.x, y = index,color=new_item,linetype=new_item), lwd = 1) +
  scale_color_manual(values = c( "#A285D1","#801650","#F46A25","#28A197","#28A197","#28A197"))+
  scale_linetype_manual(values=c("solid","solid","solid","solid","dotdash","dashed"))+
  guides(color=guide_legend(nrow=6, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index (100=1990)")

save_graphic(agricultural_land_use_chart, "1.2.2b", "agricultural land use")
save_csv(agricultural_land_use, "1.2.2b", "agricultural land use")


# Cropland area per capita -----------------------------------------------------

cropland_per_hectare <- aws.s3::s3read_using(FUN = read_csv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/input_data/t1_2_2/Croplandperhectare.csv")%>%
  rename(item=Item)%>%
  rename(value=Value)%>%
  rename(year=Year)

cropland_per_hectare_chart <- cropland_per_hectare|>
  ggplot() +
  geom_line(aes(x = year, y = value,group=Area,color=Area), lwd = 1) +
  # geom_point(aes(x = year, y = value,group=Area,color=Area),size=2)+
  scale_color_manual(values = af_colours("categorical"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Hectares per capita")

save_graphic(cropland_per_hectare_chart, "1.2.2d", "cropland_per_hectare")
save_csv(cropland_per_hectare, "1.2.2d", "cropland_per_hectare")
