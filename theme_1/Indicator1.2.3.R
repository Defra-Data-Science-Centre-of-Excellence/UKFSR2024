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

production_temp <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_2_4/output/csv/FAOSTAT_data_en_4-8-2024.csv")

production<-production_temp%>%
  mutate(Item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Meat of cattle",Item))%>%
  select(Year,Item,Value)

trade_temp <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_2_4/output/csv/FAOSTAT_data_en_4-8-2024 (3).csv")

trade<-trade_temp%>%
  mutate(Item=if_else(Item=="Meat of cattle boneless, fresh or chilled","Meat of cattle with the bone, fresh or chilled",Item))%>%
  mutate(Item=if_else(Item%in%c("Rice, broken","Rice, milled","Rice, milled (husked)","Rice, paddy (rice milled equivalent)"),"Rice",Item))%>%
  group_by(Year,Item)%>%
  summarise(Value=sum(Value),na.rm=TRUE)%>%
  mutate(Item=if_else(Item=="Meat of cattle with the bone, fresh or chilled","Meat of cattle",Item))%>%
  select(Year,Item,Value)

psd_temp <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_4/input/csv/psd.csv")

psd<-psd_temp%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))

psd_exports<-psd%>%
  filter(Attribute=="Exports")%>%
  pivot_longer(cols=4:67,names_to="year",values_to="value")%>%
  mutate(year=as.numeric(substr(year,1,4)))%>%
  filter(year>2013)

psd_production<-psd%>%
  filter(Attribute=="Production")%>%
  pivot_longer(cols=4:67,names_to="year",values_to="value")%>%
  mutate(year=as.numeric(substr(year,1,4)))%>%
  mutate(year=as.numeric(substr(year,1,4)))%>%
  filter(year>2013)


#########

#percentage<-trade%>%
#  left_join(production,by=c("Item"="Item","Year"="Year"))%>%
#  mutate(Per=round((Value.x/Value.y)*100,1))%>%
#  select(Year,Item,Per)%>%
#  filter(Year>2011)

percentage<-psd_exports%>%
  left_join(psd_production,by=c("Commodity"="Commodity","year"="year"))%>%
  mutate(Per=round((value.x/value.y)*100,1))%>%
  select(year,Commodity,Per)

########

#percentage_production_globally_traded_chart<-ggplot()+
#  geom_line(data=percentage,aes(x = Year, y = Per, colour = Item), lwd = 1) +
#  geom_point(data=percentage,aes(x=Year,y=Per,group=Item,color=Item,shape=Item),size=4)+
#  scale_y_continuous(limits = c(0,100)) +
#  scale_x_continuous(limits = c(2012,2022),breaks=seq(2012,2022,2)) +
#  scale_colour_manual(values = af_colours("categorical",n=6)) +
#  theme_ukfsr(base_family = "GDS Transport Website") +
#  labs(x = NULL,
#       y = "percent")

percentage_production_globally_traded_chart<-ggplot()+
  geom_line(data=percentage,aes(x = year, y = Per, colour = Commodity), lwd = 1) +
  #geom_point(data=percentage,aes(x=year,y=Per,group=Commodity,color=Commodity,shape=Commodity),size=4)+
  scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(breaks=seq(2015,2025,2),labels=c("2014/2015","2016/2017","2018/2019","2020/2021","2022/2023","2024/2025"))+
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_production_globally_traded_chart, "1.2.4", "percentage production globally traded")
