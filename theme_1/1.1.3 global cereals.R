library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(forcats)        
library(here)

source(here("utils", "load-font.R"))


cereal_production_yield <- aws.s3::s3read_using(FUN = read_csv,
                                                bucket = ukfsr::s3_bucket(),
                                                object = "theme_1/input_data/t1_1_3/cereal_yield_production.csv")%>%
  mutate(Area=factor(Area,levels=c("Europe","Asia","Africa","South America","Northern America","World")))

# Cereal production ------------------------------------------------------------

cereal_production<-cereal_production_yield%>%
  filter(Element=="Production")%>%
  filter(Area%in%c("Africa","Asia","Europe","Northern America","South America"))%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(item=Item)%>%
  select(year,area,item,value)

cereal_yield<-cereal_production_yield%>%
  filter(Element=="Yield")%>%
  #filter(Area%in%c("Africa","Asia","Europe","Northern America","South America","World"))%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(item=Item)%>%
  select(year,area,item,value)


cereal_production_chart <- cereal_production |>
  ggplot() +
  facet_wrap(~item)+
  geom_line(aes(x = year, y = value/1E6, colour = area), lwd = 1) +
  scale_x_continuous(limits = c(1970,2022),breaks =seq(1970,2022,10)) +
  guides(colour=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(cereal_production_chart, "1.1.3a", "global cereal production")
save_csv(cereal_production, "1.1.3a", "global cereal production")


# Cereal yield -----------------------------------------------------------------

cereal_yield_chart <- cereal_yield |>
  #filter(area!="Australia and New Zealand") |>
  ggplot() +
  facet_wrap(~item)+
  geom_line(aes(x = year, y = value/10000, colour = area), lwd = 1) +
  geom_point(aes(x=year, y=value/1E4,colour=area,shape=area,fill=area),size=2)+
  scale_x_continuous(limits = c(1970,2022),breaks =seq(1970,2022,10)) +
  #scale_y_continuous(limits = c(0,9),breaks =seq(0,8,2)) +
  scale_colour_manual(values = af_colours("categorical",n=6),limits=c("Europe","Asia","Africa","South America","Northern America","World")) +
  scale_shape_manual(values=c(NA,NA,NA,NA,NA,24))+
  guides(colour=guide_legend(nrow=3, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "tonnes per hectare")

save_graphic(cereal_yield_chart, "1.1.3b", "global cereal yields")
save_csv(cereal_yield, "1.1.3b", "global cereal yields")
