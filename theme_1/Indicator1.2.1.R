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
library(tidyverse)

fertiliser_concertrations <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_1/input/csv/FertiliserConcertrations.csv")

fertiliser_concertrations_world<-fertiliser_concertrations%>%
  filter(Area=="World")

fertiliser_concertrations_countries<-fertiliser_concertrations%>%
  filter(!Area=="World")

fertiliser_concertrations_out<-fertiliser_concertrations_countries%>%
  left_join(fertiliser_concertrations_world,by=c("Year"="Year","Item"="Item","Element"="Element"))%>%
  mutate(share=Value.x/Value.y)%>%
  mutate(share_squared=share*share)%>%
  group_by(Year,Item)%>%
  summarise(share_squared=sum(share_squared,na.rm = TRUE))%>%
  filter(Year>2000)%>%
  rename(year=Year)%>%
  rename(item=Item)%>%
  mutate(item=str_replace(item,"Nutrient",""))%>%
  mutate(item=str_replace(item,"\\(total\\)",""))%>%
  select(year,share_squared,item)

########

fertiliser_concertrations_chart<-ggplot()+
  geom_line(data=fertiliser_concertrations_out,aes(x = year, y = share_squared, colour = item)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "HHI")

save_graphic(fertiliser_concertrations_chart, "1.2.1", "fertiliser concertrations")
save_csv(fertiliser_concertrations_out, "1.2.1", "fertiliser concertrations")

fertiliser_production <- aws.s3::s3read_using(FUN = read_csv,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/t1_2_1/input/csv/FertilserProduction.csv")%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)

nitrogen_production<-fertiliser_production%>%
  filter(str_detect(Item,"nitrogen"))

phosphate_production<-fertiliser_production%>%
  filter(str_detect(Item,"phosphate"))

potash_production<-fertiliser_production%>%
  filter(str_detect(Item,"potash"))

nitrogen_production_chart<-ggplot()+
  geom_line(data=nitrogen_production,aes(x = year, y = value/1E6, colour = area)) +
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(nitrogen_production_chart, "1.2.1", "nitrogen production chart")
save_csv(nitrogen_production, "1.2.1", "nitrogen production")

phosphate_production_chart<-ggplot()+
  geom_line(data=phosphate_production,aes(x = year, y = value/1E6, colour = area)) +
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(phosphate_production_chart, "1.2.1", "phosphate production chart")
save_csv(phosphate_production, "1.2.1", "phosphate production")

potash_production_chart<-ggplot()+
  geom_line(data=potash_production,aes(x = year, y = value/1E6, colour = area)) +
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(potash_production_chart, "1.2.1", "potash production chart")
save_csv(potash_production, "1.2.1", "potash production")

################################

fertilizers_price_index <- aws.s3::s3read_using(FUN = read_csv,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/t1_2_1/input/csv/FertiliserPrice.csv")%>%
  mutate(date=as.Date(paste0(year,"/",month,"/",day)))%>%
  select(-day,-month,-year)
  
fertilizers_price_index_chart<-ggplot()+
  geom_line(data=fertilizers_price_index,aes(x = date, y = index)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index")


save_graphic(fertilizers_price_index_chart, "1.2.1", "fertilizers price index chart")
save_csv(fertilizers_price_index, "1.2.1", "fertilizers price index")
