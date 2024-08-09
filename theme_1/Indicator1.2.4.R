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
                                        object = "theme_1/t1_2_4/input/csv/proportion_traded.csv")

psd<-psd_temp%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))

psd_exports<-psd%>%
  filter(Attribute=="Exports")%>%
  pivot_longer(cols=5:7,names_to="year",values_to="value")%>%
  mutate(year2=as.numeric(substr(year,1,4)))

psd_production<-psd%>%
  filter(Attribute=="Production")%>%
  pivot_longer(cols=5:7,names_to="year",values_to="value")%>%
  mutate(year2=as.numeric(substr(year,1,4)))%>%
  mutate(year2=as.numeric(substr(year,1,4)))

percentage<-psd_exports%>%
  left_join(psd_production,by=c("Commodity"="Commodity","Type"="Type","year2"="year2"))%>%
  mutate(Per=round((value.x/value.y)*100,1))%>%
  mutate(Commodity=as.factor(Commodity))%>%
  select(year2,Type,Commodity,Per)%>%
  rename(year=year2)%>%
  rename(type=Type)%>%
  rename(commodity=Commodity)%>%
  rename(per=Per)


########

percentage_production_globally_traded_chart<-ggplot()+
  geom_col(data=percentage,aes(x = year, y = per,group=year,fill=as.character(year))) +
  #scale_y_continuous(limits = c(0,50)) +
  facet_wrap(~commodity)+
  scale_x_continuous(limits = c(1999,2030),breaks=seq(2004,2024,10),labels=c("04/05","14/15","24/25"))+
  scale_fill_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_production_globally_traded_chart, "1.2.4", "percentage production globally traded")
save_csv(percentage, "1.2.4", "percentage production globally traded")

neg<-function(x) -x

ifpri <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_2_4/input/csv/Food_Import_Dependence_Index.csv")%>%
  mutate(IndexCat=if_else(Index<neg(5),"exporter",if_else(Index<5,"self sufficent",if_else(Index<20,"very low",if_else(Index<30,"low",if_else(Index<40,"medium",if_else(Index>50,"high","very high")))))))
  

world_map <- aws.s3::s3read_using(FUN = read_sf,
                              bucket = ukfsr::s3_bucket(),
                              object = "theme_1/t1_2_4/input/csv/world.geo.json")

world_map_ifpri<-world_map%>%
  left_join(ifpri,by=c("sov_a3"="ISO3CODE"))%>%
  filter(Commodity=="All")

world_map_ifpri_chart<-ggplot()+ 
  geom_sf(data=world_map)+
  geom_sf(data = world_map_ifpri,aes(fill=IndexCat))+
  scale_color_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(world_map_ifpri_chart, "1.2.4", "food_import_vulnerability_index_food_import_dependence_ratio")
save_csv(ifpri, "1.2.4", "food_import_vulnerability_index_food_import_dependence_ratio.csv")

# FSI Indicator 2 --------------------------------------------------------------

source(here::here("utils", "load-font.R"))

for(i in c(14, 16, 22)) {
  
  cht <- percentage_production_globally_traded_chart +
                  scale_x_continuous(breaks=seq(2015,2025,2),
                                     labels=c("15/16","17/18","19/20","21/22","23/24","25/26")) +
                  guides(colour = guide_legend(nrow=3)) +
                 theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt"))+
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.2.1", paste("percentage production globally traded fsi base", i))
  
}

