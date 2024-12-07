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
library(scales)

source(here::here("utils", "load-font.R"))

# % production globally traded -------------------------------------------------
psd_temp <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/input_data/t1_3_3/proportion_traded.csv")

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
  rename(per=Per)%>%
  mutate(commodity=case_when(commodity=="Meat, Swine"~"Pigmeat",commodity=="Meat, Beef and Veal"~"Beef and Veal",commodity=="Oilseed, Soybean"~"Soybean",commodity=="Rice, Milled"~"Rice",commodity=="Meat, Chicken"~"Chicken",TRUE~commodity))


percentage_production_globally_traded_chart<-ggplot()+
  geom_line(data=percentage,aes(x = year, y = per/100,group=commodity),color=af_colours("duo")[1]) +
  facet_wrap(~commodity,scales="free")+
  scale_x_continuous(limits = c(2004,2024),breaks=seq(2004,2024,10),labels=c("04/05","14/15","24/25"))+
  scale_y_continuous(limits = c(0,0.5),labels=scales::percent)+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(panel.spacing = unit(1, "cm"),
        plot.margin=unit(c(0.2,1,0.2,0.2),"cm"))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  labs(x = NULL,
       y = "")

save_graphic(percentage_production_globally_traded_chart, "1.3.3a", "percentage production globally traded")
save_csv(percentage, "1.3.3a", "percentage production globally traded")


# Food import dependence ratio -------------------------------------------------
neg<-function(x) -x

ifpri <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/input_data/t1_3_3/Food_Import_Dependence_Index.csv")%>%
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
  mutate(Country=if_else(Country=="Republic of Korea","South Korea",Country))%>%
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
  mutate(IndexCat=if_else(is.na(IndexCat),"no data",IndexCat),
         IndexCat=if_else(IndexCat=="very high","high",IndexCat),
         IndexCat = factor(IndexCat,
                           levels = c("self sufficent", "very low", "low", "medium", "high", "no data"),
                           labels = c("Self sufficient (-5% - 5%)", "Very low (5%-19%)", "Low (20%-29%)", "Medium (30%-39%)", "High (40%+)", "No data")))

world_map_ifpri_chart <-
  ggplot()+
  #geom_polygon(data = world_map_ifpri,aes(x=long,y=lat,group=group))+
  geom_polygon(data = world_map_ifpri |> filter(region != "Antarctica"),aes(x=long,y=lat,group=group,fill=IndexCat))+
  scale_fill_manual(values = c("#28A197","#12436D","#A285D1","#801650","#F46A25", "grey")) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_blank())


save_graphic(world_map_ifpri_chart, "1.3.3c", "food import dependence ratio")
save_csv(ifpri, "1.3.3c", "food import dependence ratio")

# Rice prices ------------------------------------------------------------------

rice_chart_source_data_wb <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/input_data/t1_3_3/Rice_chart_source_data_WB.csv")%>%
  mutate(Date=dmy(Date))

date_1<-c(dmy("01-09-2007"),dmy("01-09-2008"))
date_2<-c(dmy("01-09-2022"),dmy("01-09-2024"))

rice_chart_source_data_wb_chart <-
  ggplot()+
  geom_area(aes(x=date_1,y=1000),fill="grey",alpha=0.6)+
  geom_area(aes(x=date_2,y=1000),fill="grey",alpha=0.6)+
  geom_vline(xintercept = dmy("23-03-2020"), col = "grey", linewidth = 2)+
  geom_vline(xintercept = dmy("24-02-2022"), col = "red", linewidth = 2)+
  geom_line(data=rice_chart_source_data_wb,aes(x = Date, y = `Thai 5% rice ($/mt)`), colour = af_colours()[1]) +
  scale_x_continuous(breaks=seq(dmy("01-01-2004"),dmy("01-01-2024"),"4 years"),labels=scales::label_date(format = "%Y"))+
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, 0.05)))+
  # scale_color_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "$/mt")

save_graphic(rice_chart_source_data_wb_chart, "1.3.3e", "wb rice nominal prices")
save_csv(rice_chart_source_data_wb, "1.3.3e", "wb rice nominal prices")

# Daily chokepoint transit trade volume-----------------------------------------

daily_chokepoint_transit_calls_and_trade_volume_estimates <- aws.s3::s3read_using(FUN = read_csv,
                                                                                  bucket = ukfsr::s3_bucket(),
                                                                                  object = "theme_1/input_data/t1_3_4/7day_moving_average_chokepoints.csv")%>%
  pivot_longer(2:4,names_to = "chokepoint",values_to="value")%>%
  mutate(date=as.Date(`Row Labels`))%>%
  filter(date>as.Date("2022-12-31"))%>%
  select(-`Row Labels`)

daily_chokepoint_transit_calls_and_trade_volume_estimates_chart<-ggplot()+
  geom_line(data=daily_chokepoint_transit_calls_and_trade_volume_estimates,aes(x=date,y=value/1E6,group=chokepoint,color=chokepoint))+
  geom_vline(data=daily_chokepoint_transit_calls_and_trade_volume_estimates,aes(xintercept = as.Date("2023-11-01")),linetype="dashed")+
  #geom_text(aes(x=as.Date("2023-08-01"),y=3,label="dashed line\nindicates start of houthi attacks"),size=6)+
  scale_y_continuous(limits = c(0,10),breaks=seq(0,9,1), expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(date_labels = "%b %y",breaks = date_breaks("3 months"))+
  scale_colour_manual(values = af_colours("categorical",n=4)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes") 

save_graphic(daily_chokepoint_transit_calls_and_trade_volume_estimates_chart, "1.3.3f", "daily chokepoint transit trade volume")
save_csv(daily_chokepoint_transit_calls_and_trade_volume_estimates, "1.3.3f", "daily chokepoint transit trade volume")

