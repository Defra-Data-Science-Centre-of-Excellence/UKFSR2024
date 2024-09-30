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
library(scales)
library(zoo)

export_shares <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_5/input/csv/export_shares.csv")%>%
  pivot_longer(cols=4:23,names_to="years",values_to = "value")%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))
  
export_shares_world <-export_shares%>%
  filter(Country=="World")

export_shares_not_world <-export_shares%>%
  filter(!Country=="World")

export_shares_global<-export_shares_not_world%>%
  left_join(export_shares_world,by=c("Commodity"="Commodity","Attribute"="Attribute","Unit Description"="Unit Description","years"="years"))%>%
  mutate(HI=(value.x/value.y)^2)%>%
  group_by(Commodity,years)%>%
  summarise(HI=sum(HI,na.rm=TRUE))%>%
  mutate(year=as.numeric(substr(years,1,4)))%>%
  arrange(desc(year))%>%
  rename(commodity=Commodity)%>%
  rename(hi=HI)

export_shares_global_a<-export_shares_not_world%>%
  left_join(export_shares_world,by=c("Commodity"="Commodity","Attribute"="Attribute","Unit Description"="Unit Description","years"="years"))%>%
  mutate(HI=(value.x/value.y)^2)%>%
  mutate(year=as.numeric(substr(years,1,4)))%>%
  arrange(desc(year))


########

herfindhal_indices_chart<-ggplot()+
  geom_line(data=export_shares_global,aes(x = year, y = signif(hi,digits=3),group=commodity, colour = commodity)) +
  geom_point(data=export_shares_global,aes(x = year, y = signif(hi,digits=3),group=commodity, colour = commodity, shape = commodity),size=3) +
  scale_y_continuous(limits = c(0,0.5),breaks=c(0.1,0.2,0.3,0.4,0.5),labels=c(0.1,0.2,0.3,0.4,0.5)) +
  scale_x_continuous(limits = c(2004,2024),breaks=c(seq(2004,2020,4),2023),labels=c("04/05","08/09","12/14","16/17","20/21","23/24"))+
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "HHI(export share squared)",colour="",linetype="",shape="",size="")

export_shares_global<-export_shares_global%>%
  select(-year)%>%
  rename(year=years)

save_graphic(herfindhal_indices_chart, "1.2.5", "herfindhal indices")
save_csv(export_shares_global, "1.2.5", "herfindhal indices")

daily_chokepoint_transit_calls_and_trade_volume_estimates <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/t1_2_5/input/csv/7day_moving_average_chokepoints.csv")%>%
  pivot_longer(2:4,names_to = "chokepoint",values_to="value")%>%
  mutate(date=as.Date(`Row Labels`))%>%
  filter(date>as.Date("2022-12-31"))%>%
  select(-`Row Labels`)

daily_chokepoint_transit_calls_and_trade_volume_estimates_chart<-ggplot()+
  geom_line(data=daily_chokepoint_transit_calls_and_trade_volume_estimates,aes(x=date,y=value/1E6,group=chokepoint,color=chokepoint))+
  geom_vline(data=daily_chokepoint_transit_calls_and_trade_volume_estimates,aes(xintercept = as.Date("2023-11-01")),linetype="dashed")+
  geom_text(aes(x=as.Date("2023-08-01"),y=3,label="dashed line\nindicates start of houthi attacks"),size=6)+
  scale_y_continuous(limits = c(0,10),breaks=seq(0,9,1)) +
  scale_x_date(date_labels = "%b %y",breaks = date_breaks("3 months"))+
  scale_colour_manual(values = af_colours("categorical",n=4)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "million tonnes") 

save_graphic(daily_chokepoint_transit_calls_and_trade_volume_estimates_chart, "1.2.5", "daily chokepoint transit calls and trade volume estimates chart")
save_csv(daily_chokepoint_transit_calls_and_trade_volume_estimates, "1.2.5", "daily chokepoint transit calls and trade volume estimates")

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

