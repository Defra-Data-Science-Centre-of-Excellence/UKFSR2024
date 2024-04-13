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

percentage<-psd_exports%>%
  left_join(psd_production,by=c("Commodity"="Commodity","year"="year"))%>%
  mutate(Per=round((value.x/value.y)*100,1))%>%
  select(year,Commodity,Per)

########

percentage_production_globally_traded_chart<-ggplot()+
  geom_line(data=percentage,aes(x = year, y = Per, colour = Commodity), lwd = 1) +
  scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(breaks=seq(2015,2025,2),labels=c("2014/2015","2016/2017","2018/2019","2020/2021","2022/2023","2024/2025"))+
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_production_globally_traded_chart, "1.2.4", "percentage production globally traded")