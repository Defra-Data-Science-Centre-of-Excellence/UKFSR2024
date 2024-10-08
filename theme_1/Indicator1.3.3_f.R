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
library(rworldmap)
library(tmap)
library(sf)

psd_temp <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_3_3/input/csv/proportion_traded.csv")

psd<-psd_temp%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))

psd_exports<-psd%>%
  filter(Attribute=="Exports")%>%
  pivot_longer(cols=4:24,names_to="year",values_to="value")%>%
  mutate(year2=as.numeric(substr(year,1,4)))

psd_production<-psd%>%
  filter(Attribute=="Production")%>%
  pivot_longer(cols=4:24,names_to="year",values_to="value")%>%
  mutate(year2=as.numeric(substr(year,1,4)))%>%
  mutate(year2=as.numeric(substr(year,1,4)))

percentage<-psd_exports%>%
  left_join(psd_production,by=c("Commodity"="Commodity","year2"="year2"))%>%
  mutate(Per=round((value.x/value.y)*100,1))%>%
  mutate(Commodity=as.factor(Commodity))%>%
  select(year2,Commodity,Per)%>%
  rename(year=year2)%>%
  #rename(type=Type)%>%
  rename(commodity=Commodity)%>%
  rename(per=Per)


########

percentage_production_globally_traded_chart<-ggplot()+
  geom_line(data=percentage,aes(x = year, y = per,group=commodity),color=af_colours("duo")[1]) +
  facet_wrap(~commodity,scales="free")+
  scale_x_continuous(limits = c(2004,2024),breaks=c(seq(2004,2018,7),2024),labels=c("04/05","11/12","18/19","24/25"))+
  scale_y_continuous(limits =c(0,50))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(panel.spacing = unit(1, "cm"),
        plot.margin=unit(c(0.2,1,0.2,0.2),"cm"))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_production_globally_traded_chart, "1.3.3", "percentage production globally traded")
save_csv(percentage, "1.3.3", "percentage production globally traded")

neg<-function(x) -x

ifpri <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_3_3/input/csv/Food_Import_Dependence_Index.csv")%>%
  mutate(IndexCat=if_else(Index<neg(5),"exporter",if_else(Index<5,"self sufficent",if_else(Index<20,"very low",if_else(Index<30,"low",if_else(Index<40,"medium",if_else(Index>50,"high","very high")))))))%>%
  mutate(Country=if_else(Country=="United States of America","USA",Country))%>%
  mutate(Country=if_else(Country=="Viet Nam","Vietnam",Country))%>%
  mutate(Country=if_else(Country=="Venezuela (Bolivarian Republic of)","Venezuela",Country))%>%
  mutate(Country=if_else(Country=="Russian Federation","Russia",Country))%>%
  mutate(Country=if_else(Country=="Antigua and Barbuda","Antigua",Country))%>%
  mutate(Country=if_else(Country=="Antigua and Barbuda","Barbuda",Country))%>%
  mutate(Country=if_else(Country=="Bolivia (Plurinational State of)","Bolivia",Country))%>%
  mutate(Country=if_else(Country=="Côte d'Ivoire","Ivory Coast",Country))%>%
  mutate(Country=if_else(Country=="Congo","Republic of Congo",Country))%>%
  mutate(Country=if_else(Country=="Micronesia (Federated States of)","Micronesia",Country))%>%
  mutate(Country=if_else(Country=="United Kingdom","UK",Country))%>% 
  mutate(Country=if_else(Country=="Iran (Islamic Republic of)","Iran",Country))%>%
  mutate(Country=if_else(Country=="Saint Kitts and Nevis","Saint Kitts",Country))%>% 
  mutate(Country=if_else(Country=="Saint Kitts and Nevis","Nevis",Country))%>%
  mutate(Country=if_else(Country=="Republic of Korea","Venezuela",Country))%>%
  mutate(Country=if_else(Country=="Lao People's Democratic Republic","Laos",Country))%>%
  mutate(Country=if_else(Country=="Republic of Moldova","Moldova",Country))%>%
  mutate(Country=if_else(Country=="The former Yugoslav Republic of Macedonia","North Macedonia",Country))%>%
  mutate(Country=if_else(Country=="Eswatini","Swaziland",Country))%>%
  mutate(Country=if_else(Country=="Syrian Arab Republic","Syria",Country))%>%
  mutate(Country=if_else(Country=="Trinidad and Tobago","Trinidad",Country))%>%
  mutate(Country=if_else(Country=="Trinidad and Tobago","Tobago",Country))%>%
  mutate(Country=if_else(Country=="United Republic of Tanzania","Tanzania",Country))%>%
  mutate(Country=if_else(Country=="Saint Vincent and the Grenadines","Saint Vincent",Country))%>%
  mutate(Country=if_else(Country=="Saint Vincent and the Grenadines","Grenadines",Country))%>%
  filter(Commodity=="Top3")
  

world_map <- map_data("world")

world_map_ifpri<-world_map%>%
  left_join(ifpri,by=c("region"="Country"))%>%
  mutate(IndexCat=if_else(is.na(IndexCat),"very low",IndexCat))
  

world_map_ifpri_chart<-ggplot()+
  geom_polygon(data = world_map_ifpri,aes(x=long,y=lat,group=group))+
  geom_polygon(data = world_map_ifpri,aes(x=long,y=lat,group=group,fill=IndexCat))+
  scale_fill_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(world_map_ifpri_chart, "1.3.3", "food_import_vulnerability_index_food_import_dependence_ratio")
save_csv(ifpri, "1.3.3", "food_import_vulnerability_index_food_import_dependence_ratio.csv")

rice_chart_source_data_wb <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_3_3/input/csv/Rice_chart_source_data_WB.csv")%>%
  mutate(Date=dmy(Date))

date_1<-c(dmy("01-09-2007"),dmy("01-09-2008"))
date_2<-c(dmy("01-09-2022"),dmy("01-05-2024"))

rice_chart_source_data_wb_chart<-ggplot()+
  geom_area(aes(x=date_1,y=1000),fill="grey",alpha=0.6)+
  geom_area(aes(x=date_2,y=1000),fill="grey",alpha=0.6)+
  geom_vline(xintercept = dmy("23-03-2020"), col = "grey", linewidth = 2)+
  geom_vline(xintercept = dmy("24-02-2022"), col = "red", linewidth = 2)+
  geom_line(data=rice_chart_source_data_wb,aes(x = Date, y = `Thai 5% rice ($/mt)`)) +
  scale_x_continuous(breaks=seq(dmy("01-01-2004"),dmy("01-01-2024"),"years"),labels=str_pad(seq(04,24,1),width=2,pad="0"))+
  scale_color_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "$/mt")

save_graphic(rice_chart_source_data_wb_chart, "1.3.3", "rice_chart_source_data_wb")
save_csv(rice_chart_source_data_wb, "1.3.3", "rice_source_data_wb")
