library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

kcalpp_temp <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/kcal_capita_day.csv")%>%
                            filter(Year<2010)

kcalppns <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/kcal_capita_day_nd.csv")

kcalpp<-rbind(kcalpp_temp,kcalppns)

kcalpp_chart <- kcalpp |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |> 
  select(year,value,element)  |>
  filter(year>2010)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  #geom_vline(xintercept =2014,linetype="dashed")+
  #geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_y_continuous(limits = c(2850,3000)) +
  scale_x_continuous(limits = c(2012,2021),breaks =seq(2012,2021,1)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/person/day")


save_graphic(kcalpp_chart, "1.1.1", "global food supply")

# FSI Indicator 1 --------------------------------------------------------------

source(here::here("utils", "load-font.R"))

fsi1 <- kcalpp |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |> 
  select(year,value,element)  |>
  filter(year>2010)|>
  mutate(element = factor(element, levels= c("Food supply (kcal/capita/day)"),
                          labels= c("Food supply (kcal/person/day)"))) |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element)) +
  # geom_vline(xintercept =2014,linetype="dashed")+
  # geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_x_continuous(breaks=seq(2011,2021,2)) +
  scale_y_continuous(limits = c(2500,3000), labels = scales::label_comma()) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/person/day")

for(i in c(14, 16,22)) {
  
  cht <- fsi1 + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.1.1", paste("global food supply fsi base", i))
  
}

# -----------------------------------------------------------------

production_index <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/Gross_per_capita_Production_Index.csv")

production_index_chart <- production_index |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  mutate(element="Gross per capita production") |>
  select(year,value,element)  |>
  filter(year>2001)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  scale_y_continuous(limits = c(60,110)) +
  scale_x_continuous(limits = c(2001.5,2022.5),breaks =seq(2002,2022,2)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production index number\n(2014-2016=100)")

save_graphic(production_index_chart, "1.1.1", "global food production index")

production_data <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_1/input/csv/production_data.csv")

production_chart <- production_data |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  ggplot() +
  geom_line(aes(x = year, y = value/1E6, colour = Item,linetype=Item), lwd = 1) +
  #scale_y_continuous(limits = c(60,110)) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_linetype_manual(values=c("solid","solid","dashed","solid","solid","solid","dotted","dotted","dashed","dotted","dashed","dotted"))+
  scale_colour_manual(values = c(af_colours("categorical"),af_colours("categorical"))) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "tonnes")

save_graphic(production_chart, "1.1.1", "global food production")

# Cereal production (tonnes)----------------------------------------------------

cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_1/output/csv/cereals_production.csv")

cereal_production_chart <- cereal_production |>
  mutate(Item=if_else(Item=="Maize (corn)","Maize",Item)) |>
  mutate(Item=if_else(Item=="Cereals, primary","Cereals",Item)) |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1e6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(cereal_production_chart, "1.1.1", "global cereal production")


# FSI Indicator 1.2 cereal production per capita -------------------------------

source(here::here("utils", "load-font.R"))

cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_1_1/output/csv/cereals_production.csv")



fsi1a <- cereal_production |>
  mutate(Item=if_else(Item=="Maize (corn)","Maize",Item)) |>
  mutate(Item=if_else(Item=="Cereals, primary","All Cereals",Item)) |>
  mutate(Item=as.factor(Item)) |>
  ggplot() +
  geom_line(aes(x = Year, y = Value2, colour = Item)) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,2)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("All Cereals","Maize","Rice","Wheat","Soya beans","Barley")) +
  labs(x = NULL,
       y = "kg per person per year") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.key.width = unit(58, "pt"))

for(i in c(14, 16, 22)) {
  
  cht <- fsi1a + theme_ukfsr(base_family = "GDS Transport Website",
                            base_size = i,
                            chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.1.1a", paste("cereal production per capita fsi base", i))
  
}



# ------------------------------------------------------------------------------

use_of_agricultural_commodities_by_type_and_region <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_1_1/input/csv/use_of_agricultural_commodities_by_type_and_region.csv")

use_of_agricultural_commodities_by_type_and_region_chart <- use_of_agricultural_commodities_by_type_and_region |>
  pivot_longer(cols=c('food','feed','biofuel','other'),names_to = "item",values_to = "value") |>
  ggplot(aes(fill=item, y=value, x=commodity)) +
  geom_bar(position = "fill", stat="identity")+
  coord_flip()+
  scale_fill_manual(values = af_colours("categorical")) +
  scale_y_continuous(labels=c("0","25%","50%","75%","100%"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(use_of_agricultural_commodities_by_type_and_region_chart, "1.1.1", "use of agricultural commodities by type and region")



feed_demand_by_component_and_by_region <- aws.s3::s3read_using(FUN = read_csv,
                                                                           bucket = ukfsr::s3_bucket(),
                                                                           object = "theme_1/t1_1_1/input/csv/Feed demand by component and by region.csv")

feed_demand_by_component_and_by_region_chart <- feed_demand_by_component_and_by_region |>
  ggplot(aes(fill=`feed type`, y=value, x=area)) +
  geom_bar(position = "stack", stat="identity")+
  coord_flip()+
  scale_fill_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(feed_demand_by_component_and_by_region_chart, "1.1.1", "feed demand by component and by region")

global_cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                                               bucket = ukfsr::s3_bucket(),
                                                               object = "theme_1/t1_1_1/input/csv/cerealsproduction.csv")

global_cereal_production_chart <- ggplot(data=global_cereal_production) +
  geom_line(aes(x=year ,y=value/1E3,color=commodity,linetype=commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_x_continuous(breaks = c(2015,2017,2019,2021,2023),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "1,000,000 Million tonnes")

save_graphic(global_cereal_production_chart, "1.1.1", "global cereal production")


global_biofuel_production_in <- aws.s3::s3read_using(FUN = read_csv,
                                                 bucket = ukfsr::s3_bucket(),
                                                 object = "theme_1/t1_1_1/input/csv/HIGH_AGLINK_2023_10052024183701187.csv")

global_biofuel_production<-global_biofuel_production_in%>%
  filter(Country=="WORLD")%>%
  select(Time,Commodity,Variable,Value)%>%
  pivot_wider(names_from = Variable,values_from = Value)%>%
  mutate(value=(`Biofuel use`/Production)*100)%>%
  filter(Time>1999 & Time<2024)

global_biofuel_production_chart <- ggplot(data=global_biofuel_production) +
  geom_line(aes(x=Time ,y=value,color=Commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(breaks = seq(2000,2023,2))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Biofuel demand share of\nglobal crop production")

save_graphic(global_biofuel_production_chart, "1.1.1", "global biofuel production")

global_protein_supply <- aws.s3::s3read_using(FUN = read_csv,
                                                  bucket = ukfsr::s3_bucket(),
                                                  object = "theme_1/t1_1_1/input/csv/protein_supply_per_capita.csv")

global_protein_supply_chart <- ggplot(data=global_protein_supply) +
  geom_line(aes(x=Year ,y=Value))+
  scale_color_manual(values = af_colours("duo")) +
  #scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(breaks = seq(2010,2021,1))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals per capita per day")

save_graphic(global_protein_supply_chart, "1.1.1", "global protein supply")

food_supply <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/food_supply_per_capita.csv")


gdp <- aws.s3::s3read_using(FUN = read_csv,
                                              bucket = ukfsr::s3_bucket(),
                                              object = "theme_1/t1_1_1/input/csv/GDP_2021.csv")

population <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/global_population_2021.csv")

africa <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_1_1/input/csv/africa_list.csv")

asia <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/asia_list.csv")

europe <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/europe_list.csv")

latin_america_caribbean <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/latin_america_list.csv")

north_america <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/north_america_list.csv")

oceania <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/t1_1_1/input/csv/oceania_list.csv")

world_key<-rbind(north_america,oceania,latin_america_caribbean,europe,asia,africa)

gdp_per_calorie<-food_supply%>%
  left_join(gdp,by=c("Area"="Area"))%>%
  left_join(population,by=c("Area"="Area"))%>%
  left_join(world_key,by=c("Area"="Area"))%>%
  mutate(Region=if_else(Area=="China","Asia",Region))

gdp_per_calorie_chart<-ggplot(gdp_per_calorie)+
  geom_point(aes(x=log10(Value.y),y=Value.x,size=Value.x.x/1E3,color=Region))+
  scale_x_continuous(limits=c(3,5),breaks = c(3,3.3,3.7,4,4.3,4.7,5),labels=c("$1000","$2000","$5000","$10,000","$20,000","$50,000","$100,000"))+
  scale_color_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = "GDP per capita (USD)",
       y = "kcals per capita per day")

save_graphic(gdp_per_calorie_chart, "1.1.1", "gdp per calorie chart")

feed_change_use_in <- aws.s3::s3read_using(FUN = read_csv,
                                                               bucket = ukfsr::s3_bucket(),
                                                               object = "theme_1/t1_1_1/input/csv/feed_use_change.csv")

feed_change_use<-feed_change_use_in%>%
  pivot_longer(2:6,names_to = "use",values_to = "value")%>%
  rename(income_group=`Income Group`)

feed_change_use_chart<-ggplot(feed_change_use)+
  geom_col(aes(x=income_group,y=value,fill=use),position = "dodge")+
  #scale_x_continuous(limits=c(3,5),breaks = c(3,3.3,3.7,4,4.3,4.7,5),labels=c("$1000","$2000","$5000","$10,000","$20,000","$50,000","$100,000"))+
  scale_fill_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = "income group",
       y = "percent per annum")

save_graphic(feed_change_use_chart, "1.1.1", "feed change use chart")
