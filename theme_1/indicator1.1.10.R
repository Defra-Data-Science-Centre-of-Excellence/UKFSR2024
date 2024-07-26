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


#FAOSTAT

source(here("utils", "load-font.R"))

exchange_rates_historical <- aws.s3::s3read_using(FUN = read_excel,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_10/input/csv/Exchange Rates Historical.xlsx")%>%
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
                                                object = "theme_1/t1_1_10/input/csv/CMO-Historical-Data-Monthly.csv")%>%
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
  select(Date,currency,index)

egp_usd_chart <- egp_usd |> 
  ggplot() +
  geom_line(aes(x=Date,y=index,color=currency))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("duo"))+
  #scale_x_continuous(breaks=seq(2019,2024,1),labels=seq(2019,2024,1))+
  scale_y_continuous(breaks=seq(0,3.5,0.5),limits=c(0,3.5))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "index 2019=100")

save_graphic(egp_usd_chart, "1.1.10", "egp usd wheat")
save_csv(egp_usd, "1.1.10", "egp usd wheat")
