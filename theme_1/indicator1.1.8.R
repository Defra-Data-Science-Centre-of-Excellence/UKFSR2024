### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(stringi)
library(stringr)
library(readr)
library(ukfsr)
library(afcolours)
library(here)


#FAOSTAT

source(here("utils", "load-font.R"))

agricultral_land_use <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_8/input/csv/agricultral_land_use.csv")%>%
                                         rename(item=Item)%>%
                                         rename(value=Value)%>%
                                         rename(year=Year)



agricultural_land_use_in<-agricultral_land_use%>%
                                  filter(item%in%c("Permanent meadows and pastures","Cropland","Agricultural land"))%>%
                                  filter(Area=="World")%>%
                                  select(year,item,value)

agricultural_land_use_1961<-agricultural_land_use_in%>%
                                  filter(year==1961)

agricultural_land_use<-agricultural_land_use_in%>%
  left_join(agricultural_land_use_1961,by=c("item"="item"))%>%
  mutate(index=(value.x/value.y)*100)
  
agricultural_land_use_chart <- agricultural_land_use|>
  ggplot() +
  geom_line(aes(x = year.x, y = index,color=item), lwd = 1) +
  #scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,5)) +
  scale_color_manual(values = af_colours("categorical"))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index (100=1961)")

save_graphic(agricultural_land_use_chart, "1.1.8", "agricultural_land_use")
save_csv(agricultural_land_use, "1.1.8", "agricultural_land_use")

cropland_per_hectare <- aws.s3::s3read_using(FUN = read_csv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/t1_1_8/input/csv/Croplandperhectare.csv")%>%
  rename(item=Item)%>%
  rename(value=Value)%>%
  rename(year=Year)

cropland_per_hectare_chart <- cropland_per_hectare|>
  ggplot() +
  geom_line(aes(x = year, y = value,group=Area,color=Area), lwd = 1) +
  geom_point(aes(x = year, y = value,group=Area,color=Area,shape=Area),size=2)+
  #scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,5)) +
  scale_color_manual(values = af_colours("categorical"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "ha per capita")

save_graphic(cropland_per_hectare_chart, "1.1.8", "cropland_per_hectare")
save_csv(cropland_per_hectare, "1.1.8", "cropland_per_hectare")
