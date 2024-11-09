### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)


source(here("utils", "load-font.R"))

# NOT USED GOHI-Index ----------------------------------------------------------
gohi <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_1/input_data/t1_5_2/GOHI2.csv")%>%
  mutate(country=if_else(country=="Dem. Rep. Congo","Democratic Republic of the Congo",country))%>%
  mutate(country=if_else(country=="Brunei Darussalam","Brunei",country))%>%
  mutate(country=if_else(country=="Cabo Verde","Cape Verde",country))%>%
  mutate(country=if_else(country=="Cote d'Ivoire","Ivory Coast",country))%>%
  mutate(country=if_else(country=="United Kingdom","UK",country))%>%
  mutate(country=if_else(country=="United States of America","USA",country))%>%
  mutate(country=if_else(country=="Viet Nam","Vietnam",country))

world_map <- map_data("world")

world_map_gohi<-world_map%>%
  left_join(gohi,by=c("region"="country"))%>%
  mutate(cat=if_else(Total<40,"25-40",if_else(Total<55,"40-55",if_else(Total<60,"40-55",if_else(Total<75,"60-75",">75")))))%>%
  mutate(cat=if_else(is.na(Total),"No data",cat))%>%
  mutate(cat=factor(cat,levels=c("25-40","40-55","55-60","60-75",">75","No data")))
  
gohi_chart <- ggplot()+
  geom_polygon(data = world_map |> filter(region == "Greenland", is.na(subregion)), aes(x = long, y = lat), fill = "grey90") +
  geom_polygon(data = world_map_gohi|> filter(region != "Antarctica"),aes(x=long,y=lat,group=group,fill=cat))+
  scale_fill_manual(values = c("#F46A25","#801650","#12436D","#A285D1","#28A197","grey90")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid.major.y = element_blank())


# save_graphic(gohi_chart, "1.5.2a", "global one health index")
# save_csv(gohi, "1.5.2a", "global one health index")

# bananas ----------------------------------------------------------------------

bananas <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_1/input_data/t1_5_2/banana_production.csv")%>%
  mutate(Area=as.factor(Area))%>%
  mutate(Item=as.factor(Item))

date_1<-tibble(date=c(2010,2019),height=150)

bananas_chart <- ggplot() +
  geom_area(data=date_1,aes(x=date,y=height),fill="grey",alpha=0.6)+
  geom_line(data=bananas,aes(x = Year, y = Value/1E6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_y_continuous(breaks =seq(25,150,25), limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = af_colours("duo"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "none")+
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(bananas_chart, "1.5.2a", "global banana production")
save_csv(bananas, "1.5.2a", "global banana production")
