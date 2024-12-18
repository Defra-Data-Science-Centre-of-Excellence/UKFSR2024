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

source(here::here("utils", "load-font.R"))

# Wheat prices in USD and EGP --------------------------------------------------

exchange_rates_historical <- aws.s3::s3read_using(FUN = read_excel,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/input_data/t1_3_2/Exchange Rates Historical.xlsx",skip=1)%>%
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
                                                object = "theme_1/input_data/t1_3_2/CMO-Historical-Data-Monthly.csv",skip=4)%>%
  mutate(Year=substr(`...1`,1,4))%>%
  mutate(Month=substr(`...1`,6,7))%>%
  mutate(Date=as.Date(paste0("01-",Month,"-",Year),"%d-%m-%Y"))%>%
  filter(!is.na(Date))%>%
  mutate(WHEAT_US_HRW=as.numeric(`Wheat, US HRW`))%>%
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
  geom_line(aes(x=date,y=index*100,color=currency))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("duo"))+
  #scale_x_continuous(breaks=seq(2019,2024,1),labels=seq(2019,2024,1))+
  scale_y_continuous(breaks=seq(0,400,50),limits=c(0,400), expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index (2019 = 100)")

save_graphic(egp_usd_chart, "1.3.2c", "egp usd wheat prices")
save_csv(egp_usd, "1.3.2c", "egp usd wheat prices")

# WB prices for chicken and beef -----------------------------------------------

ppi <- aws.s3::s3read_using(FUN = read_csv,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/input_data/t1_3_2/PPIACO.csv")%>%
  mutate(observation_date=as.Date(observation_date,"%d/%m/%Y"))


com_historical_data_monthly <- aws.s3::s3read_using(FUN = read_csv,
                                                    bucket = ukfsr::s3_bucket(),
                                                    object = "theme_1/input_data/t1_3_2/CMO-Historical-Data-Monthly.csv",skip=4)%>%
  mutate(Year_tmp=substr(`...1`,1,4))%>%
  mutate(Month=substr(`...1`,6,7))%>%
  mutate(Date=as.Date(paste0("01-",Month,"-",Year_tmp),"%d-%m-%Y"))%>%
  select(Date,`Beef **`,`Chicken **`,Maize,`Palm oil`,`Rice, Thai 5%`,Soybeans,`Sugar, world`,`Wheat, US HRW`)%>%
  filter(!is.na(Date))%>%
  mutate(BEEF=as.numeric(`Beef **`))%>%
  mutate(CHICKEN=as.numeric(`Chicken **`))%>%
  mutate(MAIZE=as.numeric(Maize))%>%
  mutate(PALM_OIL=as.numeric(`Palm oil`))%>%
  mutate(RICE_05=as.numeric(`Rice, Thai 5%`))%>%
  mutate(SOYBEANS=as.numeric(Soybeans))%>%
  mutate(SUGAR_WLD=as.numeric(`Sugar, world`))%>%
  mutate(WHEAT_US_HRW=as.numeric(`Wheat, US HRW`))%>%
  select(Date,BEEF,CHICKEN,MAIZE,PALM_OIL,RICE_05,SOYBEANS,SUGAR_WLD,WHEAT_US_HRW)

com_historical_data_2023 <- aws.s3::s3read_using(FUN = read_csv,
                                                    bucket = ukfsr::s3_bucket(),
                                                    object = "theme_1/input_data/t1_3_2/CMO-Historical-Data-Monthly.csv",skip=4)%>%
  mutate(Year=as.numeric(substr(`...1`,1,4)))%>%
  filter(Year==2023)%>%
  mutate(BEEF=as.numeric(`Beef **`))%>%
  mutate(CHICKEN=as.numeric(`Chicken **`))%>%
  mutate(MAIZE=as.numeric(Maize))%>%
  mutate(PALM_OIL=as.numeric(`Palm oil`))%>%
  mutate(RICE_05=as.numeric(`Rice, Thai 5%`))%>%
  mutate(SOYBEANS=as.numeric(Soybeans))%>%
  mutate(SUGAR_WLD=as.numeric(`Sugar, world`))%>%
  mutate(WHEAT_US_HRW=as.numeric(`Wheat, US HRW`))%>%
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

# date_list<-seq(as.POSIXct("1960-01-01"), as.POSIXct("2023-01-01"), "years")
date_list<-seq(as.Date("1960-01-01"), as.Date("2023-01-01"), "years")
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
  scale_x_date(breaks = as.Date(date_list_x), date_labels = "%Y") +
  scale_y_continuous(limits=c(0, 300), expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index (2023 = 100)")

save_graphic(deflated_meat_sugar_chart, "1.3.2b", "wb commodity prices for chicken and beef")
save_csv(deflated_meat_sugar, "1.3.2b", "wb commodity prices for chicken and beef")

# WB prices for cereals --------------------------------------------------------

deflated_cereals <-deflated_cereals |> 
  mutate(commodity = factor(commodity,
                            levels = c("Maize","Rice (Thai 5%)","Soybeans", "Wheat (US HRW)"),
                            labels = c("Maize","Rice (Thai 5%)","Soybeans", "Wheat (US Hard Red Winter)")))

deflated_cereals_chart <- deflated_cereals |> 
  ggplot() +
  geom_line(data=deflated_cereals,aes(x=Date,y=value,color=commodity),linewidth=0.8)+
  #geom_point(data=deflated_cereals_5yr,aes(x=Date,y=value,color=commodity,shape=commodity),size=2)+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("categorical",n=4))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  # scale_x_continuous(breaks=date_list_x,labels = seq(1960,2020,10))+
  scale_x_date(breaks = as.Date(date_list_x), date_labels = "%Y") +
  scale_y_continuous(limits=c(0,600), breaks = seq(0, 600, 100), expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index (2023 = 100)")

save_graphic(deflated_cereals_chart, "1.3.2a", "wb commodity prices for cereals")
save_csv(deflated_cereals, "1.3.2a", "wb commodity prices for cereals")

