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



agricultural_land_use_permanent<-agricultral_land_use%>%
                                  filter(item%in%c("Permanent meadows and pastures","Permanent crops","Arable land","Cropland","Agriculture","Agricultural land"))%>%
                                  filter(Area=="World")%>%
                                  filter(year>2001)%>%
                                  mutate(per=(value/4787551.8)*100)%>%
                                  select(year,item,value,per)
  
agricultural_land_use_temporary<-agricultral_land_use%>%
                                  filter(item%in%c("Temporary crops","Temporary fallow","Temporary meadows and pastures"))%>%
                                  filter(Area=="World")%>%
                                  filter(year>2001)%>%
                                  mutate(per=(value/4787551.8)*100)%>%
                                  select(year,item,value,per)




agricultural_land_use_permanent_chart <- agricultural_land_use_permanent|>
  ggplot() +
  geom_line(aes(x = year, y = value,color=item), lwd = 1) +
  #scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,5)) +
  scale_color_manual(values = af_colours("categorical"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "million ha")

save_graphic(agricultural_land_use_permanent_chart, "1.1.8", "agricultural_land_use_permanent")
save_csv(agricultural_land_use_permanent, "1.1.8", "agricultural_land_use_permanent")

agricultural_land_use_temporary_chart <- agricultural_land_use_temporary|>
  ggplot() +
  geom_line(aes(x = year, y = value/1E3,color=item), lwd = 1) +
  #scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,5)) +
  scale_color_manual(values = af_colours("categorical"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "million ha")

save_graphic(agricultural_land_use_temporary_chart, "1.1.8", "agricultural_land_use_temporary")
save_csv(agricultural_land_use_temporary, "1.1.8", "agricultural_land_use_temporary")
