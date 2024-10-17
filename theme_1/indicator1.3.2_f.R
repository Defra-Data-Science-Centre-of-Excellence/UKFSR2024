### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(stringi)
library(stringr)
library(readr)
library(ukfsr)
library(readxl)
library(afcolours)
library(here)
library(lubridate)


#FAOSTAT

source(here("utils", "load-font.R"))

exchange_rates_historical <- aws.s3::s3read_using(FUN = read_excel,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_3_2/input/csv/Exchange Rates Historical.xlsx")%>%
  mutate(Date=as.Date(Date,"%d/%m/%Y"))


first_day<-exchange_rates_historical%>%
  mutate(year=year(Date))%>%
  mutate(month=month(Date))%>%
  mutate(day=day(Date))%>%
    group_by(year,month)%>%
    summarise(day=min(day))%>%
    mutate(Date=as.Date(paste0(year,"/",month,"/",day)))%>%
    left_join(exchange_rates_historical,by=c("Date"="Date"))%>%
  mutate(Date=as.Date(paste0(year,"/",month,"/01")))

com_historical_data_monthly <- aws.s3::s3read_using(FUN = read_csv,
                                                bucket = ukfsr::s3_bucket(),
                                                object = "theme_1/t1_3_2/input/csv/CMO-Historical-Data-Monthly.csv")%>%
  mutate(Year_tmp=substr(Year,1,4))%>%
  mutate(Month=substr(Year,6,7))%>%
  mutate(Date=as.Date(paste0("01-",Month,"-",Year),"%d-%m-%Y"))%>%
  select(Date,WHEAT_US_HRW)

egp_usd<-com_historical_data_monthly%>%
  left_join(first_day,by=c("Date"="Date"))%>%
  filter(!is.na(Currency))%>%
  mutate(Buy=as.numeric(Buy))%>%
  mutate(WHEAT_EGP=WHEAT_US_HRW*Buy)%>%
  mutate(EGP=WHEAT_EGP/3749.473)%>%
  mutate(USD=WHEAT_US_HRW/209.81)%>%
  pivot_longer(cols=10:11,values_to = "index",names_to = "currency")%>%
  rename(date=Date)%>%
  select(date,currency,index)

egp_usd_chart <- egp_usd |> 
  ggplot() +
  geom_line(aes(x=date,y=index,color=currency))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("duo"))+
  #scale_x_continuous(breaks=seq(2019,2024,1),labels=seq(2019,2024,1))+
  scale_y_continuous(breaks=seq(0,3.5,0.5),limits=c(0,3.5))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index 2019=100")

save_graphic(egp_usd_chart, "1.3.1", "egp usd wheat")
save_csv(egp_usd, "1.3.1", "egp usd wheat")

ppi <- aws.s3::s3read_using(FUN = read_excel,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/t1_3_2/input/csv/PPI_2023_rebase.xlsx",skip=10)


com_historical_data_monthly <- aws.s3::s3read_using(FUN = read_csv,
                                                    bucket = ukfsr::s3_bucket(),
                                                    object = "theme_1/t1_3_2/input/csv/CMO-Historical-Data-Monthly.csv")%>%
  mutate(Year_tmp=substr(Year,1,4))%>%
  mutate(Month=substr(Year,6,7))%>%
  mutate(Date=as.Date(paste0("01-",Month,"-",Year),"%d-%m-%Y"))%>%
  select(Date,BEEF,CHICKEN,MAIZE,PALM_OIL,RICE_05,SOYBEANS,SUGAR_WLD,WHEAT_US_HRW)

com_historical_data_2023 <- aws.s3::s3read_using(FUN = read_csv,
                                                    bucket = ukfsr::s3_bucket(),
                                                    object = "theme_1/t1_3_2/input/csv/CMO-Historical-Data-Monthly.csv")%>%
  mutate(Year=as.numeric(substr(Year,1,4)))%>%
  filter(Year==2023)%>%
  group_by(Year)%>%
  summarise(BEEF=mean(BEEF,na.rm=TRUE),CHICKEN=mean(CHICKEN,na.rm=TRUE),MAIZE=mean(MAIZE,na.rm=TRUE),PALM_OIL=mean(PALM_OIL,na.rm=TRUE),RICE_05=mean(RICE_05,na.rm=TRUE),SOYBEANS=mean(SOYBEANS,na.rm=TRUE),SUGAR_WLD=mean(SUGAR_WLD,na.rm=TRUE),WHEAT_US_HRW=mean(WHEAT_US_HRW,na.rm = TRUE))

deflated<-com_historical_data_monthly%>%
  left_join(ppi,by=c("Date"="observation_date"))%>%
  mutate(`Wheat (US HRW)`=((WHEAT_US_HRW/Deflator)/340.43)*100)%>%
  mutate(Beef=((BEEF/Deflator)/4.901667)*100)%>%
  mutate(Chicken=((CHICKEN/Deflator)/1.531667)*100)%>%
  mutate(Maize=((MAIZE/Deflator)/252.6567)*100)%>%
  mutate(`Palm Oil`=((PALM_OIL/Deflator)/886.4533)*100)%>%
  mutate(`Rice (Thai 5%)`=((RICE_05/Deflator)/553.6667)*100)%>%
  mutate(Soybeans=((SOYBEANS/Deflator)/597.8983)*100)%>%
  mutate(Sugar=((SUGAR_WLD/Deflator)/0.5166667)*100)%>%
  filter(!is.na(Deflator))%>%
  select(-PPIACO,-Deflator)%>%
  pivot_longer(cols=2:17,values_to = "value",names_to = "commodity")

deflated_meat_sugar<-deflated%>%
  filter(commodity%in%c("Beef","Chicken"))

deflated_cereals<-deflated%>%
  filter(commodity%in%c("Wheat (US HRW)","Maize","Rice (Thai 5%)","Soybeans"))

date_list<-seq(as.POSIXct("1960-01-01"), as.POSIXct("2023-01-01"), "years")
idx<-c(1,11,21,31,41,51,61)
date_list_x<-date_list[idx]


deflated_cereals_5yr<-deflated%>%
  filter(Date%in%date_list)%>%
  filter(commodity%in%c("Wheat (US HRW)","Maize","Rice (Thai 5%)","Soybeans"))

deflated_meat_sugar_chart <- deflated_meat_sugar |> 
  ggplot() +
  geom_line(aes(x=Date,y=value,color=commodity))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("categorical",n=3))+
  #scale_x_continuous(breaks=seq(2019,2024,1),labels=seq(2019,2024,1))+
  #scale_y_continuous(breaks=seq(0,3.5,0.5),limits=c(0,3.5))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "US$/kg index real 100=2023")

save_graphic(deflated_meat_sugar_chart, "1.3.2", "deflated meat sugar chart")
save_csv(deflated_meat_sugar, "1.3.2", "deflated meat sugar")

deflated_cereals_chart <-deflated_cereals |> 
  ggplot() +
  geom_line(data=deflated_cereals,aes(x=Date,y=value,color=commodity),linewidth=0.8)+
  #geom_point(data=deflated_cereals_5yr,aes(x=Date,y=value,color=commodity,shape=commodity),size=2)+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("categorical",n=4))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_x_continuous(breaks=date_list_x,labels = seq(1960,2020,10))+
  #scale_y_continuous(breaks=seq(0,3.5,0.5),limits=c(0,3.5))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "US$/mt index,real 100=2023")

save_graphic(deflated_cereals_chart, "1.3.2", "deflated cereals chart")
save_csv(deflated_cereals, "1.3.2", "deflated cereals")
