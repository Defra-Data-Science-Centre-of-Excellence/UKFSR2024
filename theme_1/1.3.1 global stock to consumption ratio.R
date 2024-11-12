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

stocks_to_consumption <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/input_data/t1_3_1/stocks_to_consumption.csv")%>%
  pivot_longer(cols = 4:24,names_to="year",values_to = "value")


stocks <- stocks_to_consumption%>%
  filter(Attribute=="Ending Stocks")

consumption <- stocks_to_consumption%>%
  filter(Attribute=="Domestic Consumption")

scr_temp<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))

scr_world<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Country%in%c("World"))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  select(Commodity,year,value.x,value.y,SCR)%>%
  mutate(Area="World")

scr_world_minus_china<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(!Country%in%c("World","China"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="World minus China")

scr_top_exporters_maize<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Commodity=="Corn")%>%
  filter(Country%in%c("Argentina","Brazil","European Union","Paraguay","Russia","South Africa","Ukraine","United States"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="Top Exporters")

scr_top_exporters_soyabean<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Commodity=="Oilseed, Soybean")%>%
  filter(Country%in%c("Argentina","Brazil","Canada","Paraguay","Russia","Ukraine","United States","Uruguay"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="Top Exporters")

scr_top_exporters_sunflowerseed<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Commodity=="Oilseed, Sunflowerseed")%>%
  filter(Country%in%c("Argentina","China","European Union","Kazakhstan","Moldova","Russia","Turkey","Ukraine"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="Top Exporters")

scr_top_exporters_rice<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Commodity=="Rice, Milled")%>%
  filter(Country%in%c("Burma","Cambodia","China","India","Pakistan","Thailand","United States","Vietnam"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="Top Exporters")
  
scr_top_exporters_wheat<-stocks%>%
  left_join(consumption,by=c("Commodity"="Commodity","Country"="Country","year"="year"))%>%
  filter(Commodity=="Wheat")%>%
  filter(Country%in%c("Argentina","Australia","Canada","European Union","Kazakhstan","Russia","Ukraine","United States"))%>%
  group_by(Commodity,year)%>%
  summarise(value.x=sum(value.x,na.rm = TRUE),value.y=sum(value.y,na.rm = TRUE))%>%
  mutate(SCR=(value.x/value.y)*100)%>%
  mutate(Area="Top Exporters")

scr_out<-rbind(scr_world,scr_world_minus_china,scr_top_exporters_maize,scr_top_exporters_soyabean,scr_top_exporters_sunflowerseed,scr_top_exporters_rice,scr_top_exporters_wheat)%>%
  mutate(plot_year=as.numeric(substr(year,1,4)))%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))%>%
  mutate(Commodity=if_else(Commodity=="Oilseed, Soybean","Soybean",Commodity))%>%
  mutate(Commodity=if_else(Commodity=="Rice, Milled","Rice",Commodity))%>%
  rename(commodity="Commodity")%>%
  rename(scr="SCR")%>%
  rename(area="Area")%>%
  rename(stocks="value.x")%>%
  rename(comsumption="value.y")%>%
  filter(commodity!="Oilseed, Sunflowerseed")
  
year_labels_temp<-tibble(year1=seq(4,24,5),year2=seq(5,25,5))%>%
  mutate(labels=paste0(str_pad(year1,width=2,pad="0"),"/",str_pad(year2,width=2,pad="0")))

year_labels<-year_labels_temp$labels


scr_chart <- scr_out |> 
  ggplot() +
  facet_wrap(~commodity,scales="free")+
  geom_line(aes(x=plot_year,y=scr/100,color=area))+
  scale_color_manual(values = af_colours("categorical"))+
  scale_x_continuous(breaks=seq(2004,2024,5),labels=year_labels)+
  scale_y_continuous(breaks=seq(0,0.6,0.1),limits=c(0,0.6),labels = scales::percent, expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(panel.spacing=unit(2,"lines"),
        # axis.ticks = element_line() ,
        axis.ticks.length = unit(.1, "cm"),
        axis.line.x = element_line(),
        plot.margin=unit(c(0.4,1,0.4,0.5),"cm"))+
  labs(x = NULL,
       y = "")

scr_out<-scr_out%>%select(-plot_year)

save_graphic(scr_chart, "1.3.1a", "stocks to consumption ratio")
save_csv(scr_out, "1.3.1a", "stock to consumption ratio")





