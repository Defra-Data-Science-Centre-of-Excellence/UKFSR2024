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

source(here("utils", "load-font.R"))


# Fertiliser concentration -----------------------------------------------------

fertiliser_concertrations <- aws.s3::s3read_using(FUN = read_csv,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/input_data/t1_2_3/FertiliserConcertrations.csv")

fertiliser_concertrations_world<-fertiliser_concertrations%>%
  filter(Area=="World")

fertiliser_concertrations_countries<-fertiliser_concertrations%>%
  filter(!Area=="World")

fertiliser_concertrations_out<-fertiliser_concertrations_countries%>%
  left_join(fertiliser_concertrations_world,by=c("Year"="Year","Item"="Item","Element"="Element"))%>%
  mutate(share=Value.x/Value.y)%>%
  mutate(share_squared=share*share)%>%
  group_by(Year,Item)%>%
  summarise(share_squared=sum(share_squared,na.rm = TRUE))%>%
  filter(Year>2000)%>%
  rename(year=Year)%>%
  rename(item=Item)%>%
  mutate(item=str_replace(item,"Nutrient",""))%>%
  mutate(item=str_replace(item,"\\(total\\)",""))%>%
  select(year,share_squared,item)


fertiliser_concertrations_chart<-ggplot()+
  geom_line(data=fertiliser_concertrations_out,aes(x = year, y = share_squared, colour = item)) +
  scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "HHI")

save_graphic(fertiliser_concertrations_chart, "1.2.3c", "fertiliser concentrations")
save_csv(fertiliser_concertrations_out, "1.2.3c", "fertiliser concentrations")


# Fertiliser production --------------------------------------------------------

fertiliser_production <- aws.s3::s3read_using(FUN = read_csv,
                                              bucket = ukfsr::s3_bucket(),
                                              object = "theme_1/input_data/t1_2_3/FertilserProduction.csv")%>%
  rename(area=Area)%>%
  rename(year=Year)%>%
  rename(value=Value)

total_production<-fertiliser_production%>%
  rename(item=Item)%>%
  mutate(item=str_replace(item,"Nutrient",""))%>%
  mutate(item=str_replace(item,"\\(total\\)",""))%>%
  filter(area=="World")

total_production_chart<-ggplot()+
  geom_line(data=total_production,aes(x = year, y = value/1E6, colour = item)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  # guides(color=guide_legend(nrow=3, byrow=TRUE))+ 
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(total_production_chart, "1.2.3a", "global fertiliser production")
save_csv(total_production, "1.2.3a", "global fertiliser production")

# Fertiliser prices ------------------------------------------------------------

fertilizers_price_index <- aws.s3::s3read_using(FUN = read_excel,
                                                bucket = ukfsr::s3_bucket(),
                                                object = "theme_1/input_data/t1_2_3/external-data.xls",
                                                skip=3)%>%
  select(Frequency,`Monthly...15`)%>%
  mutate(year=substr(Frequency,1,4))%>%
  mutate(month=substr(Frequency,6,7))%>%
  mutate(date=dmy(paste0("01-",str_pad(month, width=2, pad="0"),"-",year)))%>%
  filter(date>dmy("01-09-2004"))%>%
  rename(fertilizers_price_index=`Monthly...15`)%>%
  select(date,fertilizers_price_index)

fertilizers_price_index_chart<-ggplot()+
  geom_line(data=fertilizers_price_index,aes(x = date, y = fertilizers_price_index), colour = af_colours()[1]) +
  scale_colour_manual(values = af_colours()[1]) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Index=2016")


save_graphic(fertilizers_price_index_chart, "1.2.3b", "fertilizers price index")
save_csv(fertilizers_price_index, "1.2.3b", "fertilizers price index")
