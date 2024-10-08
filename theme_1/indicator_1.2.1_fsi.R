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

