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
                            object = "theme_1/t1_1_1/input/csv/kcal_capita_day.csv")%>%
                            filter(Year<2010)

kcalppns <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/kcal_capita_day_nd.csv")

kcalpp<-rbind(kcalpp_temp,kcalppns)%>%
  select(Year,Value,Element)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(element=Element)%>%
  filter(year>2010)

kcalpp_chart <- kcalpp |> 
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
save_csv(kcalpp, "1.1.1", "global food supply")

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
                            object = "theme_1/t1_1_1/input/csv/Gross_per_capita_Production_Index.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  mutate(element="Gross per capita production") |>
  select(year,value,element)  |>
  filter(year>2001)

production_index_chart <- production_index |>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  scale_y_continuous(limits = c(60,110)) +
  scale_x_continuous(limits = c(2001.5,2022.5),breaks =seq(2002,2022,2)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production index number\n(2014-2016=100)")

save_graphic(production_index_chart, "1.1.1", "global food production index")
save_csv(production_index, "1.1.1", "global food production index")

# production_data <- aws.s3::s3read_using(FUN = read_csv,
#                                          bucket = ukfsr::s3_bucket(),
#                                          object = "theme_1/t1_1_1/input/csv/production_data.csv")
# 
# production_chart <- production_data |> 
#   rename(year=Year) |>
#   rename(value=Value) |> 
#   rename(element=Element) |>
#   ggplot() +
#   geom_line(aes(x = year, y = value/1E6, colour = Item,linetype=Item), lwd = 1) +
#   #scale_y_continuous(limits = c(60,110)) +
#   scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
#   scale_linetype_manual(values=c("solid","solid","dashed","solid","solid","solid","dotted","dotted","dashed","dotted","dashed","dotted"))+
#   scale_colour_manual(values = c(af_colours("categorical"),af_colours("categorical"))) +
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   labs(x = NULL,
#        y = "tonnes")
# 
# save_graphic(production_chart, "1.1.1", "global food production")
# save_csv(production_data, "1.1.1", "global food production")

# Cereal production (tonnes)----------------------------------------------------

# cereal_production <- aws.s3::s3read_using(FUN = read_csv,
#                                          bucket = ukfsr::s3_bucket(),
#                                          object = "theme_1/t1_1_1/output/csv/cereals_production.csv")
# 
# cereal_production_chart <- cereal_production |>
#   mutate(Item=if_else(Item=="Maize (corn)","Maize",Item)) |>
#   mutate(Item=if_else(Item=="Cereals, primary","Cereals",Item)) |>
#   ggplot() +
#   geom_line(aes(x = Year, y = Value/1e6, colour = Item), lwd = 1) +
#   scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
#   scale_colour_manual(values = af_colours("categorical")) +
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   labs(x = NULL,
#        y = "Million Tonnes")
# 
# save_graphic(cereal_production_chart, "1.1.1", "global cereal production")
# save_csv(cereal_production_data, "1.1.1", "global cereal production")

# FSI Indicator 1.2 cereal production per capita -------------------------------

source(here::here("utils", "load-font.R"))

cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_1_1/input/csv/cereals_production.csv")



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
                                          object = "theme_1/t1_1_1/input/csv/use_of_agricultural_commodities_by_type_and_region.csv")%>%
  pivot_longer(cols=c('food','feed','biofuel','other'),names_to = "item",values_to = "value")

use_of_agricultural_commodities_by_type_and_region_chart <- use_of_agricultural_commodities_by_type_and_region |>
  ggplot(aes(fill=item, y=value, x=commodity)) +
  geom_bar(position = "fill", stat="identity")+
  coord_flip()+
  scale_fill_manual(values = af_colours("categorical")) +
  scale_y_continuous(labels=c("0","25%","50%","75%","100%"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(use_of_agricultural_commodities_by_type_and_region_chart, "1.1.1", "use of agricultural commodities by type and region")
save_csv(use_of_agricultural_commodities_by_type_and_region, "1.1.1", "use of agricultural commodities by type and region")


# feed_demand_by_component_and_by_region <- aws.s3::s3read_using(FUN = read_csv,
#                                                                            bucket = ukfsr::s3_bucket(),
#                                                                            object = "theme_1/t1_1_1/input/csv/Feed demand by component and by region.csv")
# 
# feed_demand_by_component_and_by_region_chart <- feed_demand_by_component_and_by_region |>
#   ggplot(aes(fill=`feed type`, y=value, x=area)) +
#   geom_bar(position = "stack", stat="identity")+
#   coord_flip()+
#   scale_fill_manual(values = af_colours("categorical")) +
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   labs(x = NULL,
#        y = "Million tonnes")
# 
# save_graphic(feed_demand_by_component_and_by_region_chart, "1.1.1", "feed demand by component and by region")
# 
# global_cereal_production <- aws.s3::s3read_using(FUN = read_csv,
#                                                                bucket = ukfsr::s3_bucket(),
#                                                                object = "theme_1/t1_1_1/input/csv/cerealsproduction.csv")
# 
# global_cereal_production_chart <- ggplot(data=global_cereal_production) +
#   geom_line(aes(x=year ,y=value/1E3,color=commodity,linetype=commodity))+
#   scale_color_manual(values = af_colours("categorical")) +
#   scale_x_continuous(breaks = c(2015,2017,2019,2021,2023),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   labs(x = NULL,
#        y = "1,000,000 Million tonnes")
# 
# save_graphic(global_cereal_production_chart, "1.1.1", "global cereal production")
# 

global_biofuel_production_in <- aws.s3::s3read_using(FUN = read_csv,
                                                 bucket = ukfsr::s3_bucket(),
                                                 object = "theme_1/t1_1_1/input/csv/HIGH_AGLINK_2023_10052024183701187.csv")

global_biofuel_production<-global_biofuel_production_in%>%
  filter(Country=="WORLD")%>%
  rename(time=Time)%>%
  rename(commodity=Commodity)%>%
  rename(variable=Variable)%>%
  rename(value=Value)%>%
  select(time,commodity,variable,value)%>%
  pivot_wider(names_from = variable,values_from = value)%>%
  mutate(value=(`Biofuel use`/Production)*100)%>%
  filter(time>1999 & time<2024)

global_biofuel_production_chart <- ggplot(data=global_biofuel_production) +
  geom_line(aes(x=time ,y=value,color=commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(breaks = seq(2000,2023,2))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Biofuel demand share of\nglobal crop production")

save_graphic(global_biofuel_production_chart, "1.1.1", "global biofuel production")
save_csv(global_biofuel_production, "1.1.1", "global_biofuel_production")


#global_protein_supply <- aws.s3::s3read_using(FUN = read_csv,
#                                                  bucket = ukfsr::s3_bucket(),
#                                                  object = "theme_1/t1_1_1/input/csv/protein_supply_per_capita.csv")

#global_protein_supply_chart <- ggplot(data=global_protein_supply) +
#  geom_line(aes(x=Year ,y=Value))+
#  scale_color_manual(values = af_colours("duo")) +
#  #scale_y_continuous(limits=c(0,30))+
#  scale_x_continuous(breaks = seq(2010,2021,1))+
#  theme_ukfsr(base_family = "GDS Transport Website") +
#  labs(x = NULL,
#       y = "kcals per capita per day")

#save_graphic(global_protein_supply_chart, "1.1.1", "global protein supply")

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
  geom_hline(aes(yintercept = 2000, linetype="dashed"))+
  geom_hline(aes(yintercept = 2500, linetype="dashed"))+
  scale_x_continuous(limits=c(3,5),breaks = c(3,3.3,3.7,4,4.3,4.7,5),labels=c("$1000","$2000","$5000","$10,000","$20,000","$50,000","$100,000"))+
  scale_color_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = "GDP per capita (USD)",
       y = "kcals per capita per day")

save_graphic(gdp_per_calorie_chart, "1.1.1", "gdp per calorie chart")
save_csv(gdp_per_calorie, "1.1.1", "gdp per calorie")

# feed_change_use_in <- aws.s3::s3read_using(FUN = read_csv,
#                                                                bucket = ukfsr::s3_bucket(),
#                                                                object = "theme_1/t1_1_1/input/csv/feed_use_change.csv")
# 
# feed_change_use<-feed_change_use_in%>%
#   pivot_longer(2:6,names_to = "use",values_to = "value")%>%
#   rename(income_group=`Income Group`)
# 
# feed_change_use_chart<-ggplot(feed_change_use)+
#   geom_col(aes(x=income_group,y=value,fill=use),position = "dodge")+
#   #scale_x_continuous(limits=c(3,5),breaks = c(3,3.3,3.7,4,4.3,4.7,5),labels=c("$1000","$2000","$5000","$10,000","$20,000","$50,000","$100,000"))+
#   scale_fill_manual(values = af_colours("categorical")) +
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   labs(x = "income group",
#        y = "percent per annum")
# 
# save_graphic(feed_change_use_chart, "1.1.1", "feed change use chart")

production <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_1/input/csv/production.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value,item)

cereals_production <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_1_1/input/csv/cereals_production.csv")%>%
  filter(Area=="World") |>
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value,item)

population <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_1_1/input/csv/global_population_2022.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value)

production_per_capita<-production%>%
  left_join(population,by=c("year"="year"))%>%
  mutate(value=(((value.x)/value.y)*1000)/365)%>%
  mutate(item=if_else(item%in%c("Citrus Fruit, Total","Fruit Primary","Vegetables Primary"),"Fruit and Vegetables, Primary + Citrus Fruit",item))%>%
  mutate(item=as.factor(item))%>%
  mutate(item=ordered(item,levels=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit")))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))#%>%
  #filter(item!="Eggs Primary")

cereals_per_capita<-cereals_production%>%
  left_join(population,by=c("year"="year"))%>%
  mutate(value=((value.x/value.y)))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))    

production_chart <- production_per_capita|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = item), lwd = 1) +
  geom_point(aes(x = year, y = value, colour = item, shape = item),size=3) +
  scale_x_continuous(limits = c(1959.5,2022.5),breaks =seq(1960,2022,10)) +
  scale_colour_manual(values = af_colours("categorical",n=6))+#,limits=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit"))+
  scale_shape_manual(values = c(16,32,32,32,32,32))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production g per capita per day",
       shape="",
       colour="")

save_graphic(production_chart, "1.1.1", "global food production")
save_csv(production, "1.1.1", "global food production")

food_supply_2 <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_1_1/input/csv/food_supply_output.csv")

food_supply_2_chart<-ggplot(food_supply_2)+
  geom_line(aes(x=Year,y=Value),color=af_colours("duo")[1])+
  geom_vline(aes(xintercept = 2010),linetype="dashed")+
  facet_wrap(~Area)+#,scales="free")+
  #annotate("text",x=2000,y=1500,size=6,label="change in\nmethodology")+
  scale_y_continuous(limits=c(0,4000))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(panel.spacing = unit(1, "cm"),
        plot.margin=unit(c(0.2,1,0.2,0.2),"cm"))+
  labs(x = "Year",
       y = "kcals per capita per day")

save_graphic(food_supply_2_chart, "1.1.1", "global food supply")
save_csv(food_supply_2, "1.1.1", "global food supply")

production_ <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_1_1/input/csv/production_.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value,item)

production_per_capita_<-production_%>%
  left_join(population,by=c("year"="year"))%>%
  mutate(value=(((value.x)/value.y)*1000)/365)%>%
  #mutate(item=if_else(item%in%c("Citrus Fruit, Total","Fruit Primary","Vegetables Primary"),"Fruit and Vegetables, Primary + Citrus Fruit",item))%>%
  #mutate(item=as.factor(item))%>%
  #mutate(item=ordered(item,levels=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit")))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))

production_per_capita_cereals<-production_per_capita_%>%
  filter(item=="Cereals, primary")%>%
  filter(year>2009)

production_per_capita_sugar<-production_per_capita_%>%
  filter(item=="Sugar Crops Primary")

supply_ <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_1_1/input/csv/supply_.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  mutate(value=(value*1000)/365)|>
  select(year,value,item)

supply_cereals<-supply_%>%
  filter(item=="Cereals - Excluding Beer")%>%
  filter(year>2009)

supply_sugar<-supply_%>%
  filter(item=="Sugar Crops")

ggplot()+
geom_line(data=production_per_capita_cereals,aes(x=year,y=value))+
geom_line(data=supply_cereals,aes(x=year,y=value))

ggplot()+
geom_line(data=production_per_capita_sugar,aes(x=year,y=value))+
geom_line(data=supply_sugar,aes(x=year,y=value))


supply_sugar<-supply_%>%
  filter(item=="Sugar Crop")

supply_wheat_rice_maize <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_1/t1_1_1/input/csv/wheat_rice_maize_supply.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  mutate(value=(value*1000)/365)|>
  select(year,value,item)

wheat_rice_maize_production <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/t1_1_1/input/csv/wheat_rice_maize_production.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value,item)

production_per_capita_wheat_rice_maize<-wheat_rice_maize_production%>%
  left_join(population,by=c("year"="year"))%>%
  mutate(value=(((value.x)/value.y)*1000)/365)%>%
  #mutate(item=if_else(item%in%c("Citrus Fruit, Total","Fruit Primary","Vegetables Primary"),"Fruit and Vegetables, Primary + Citrus Fruit",item))%>%
  #mutate(item=as.factor(item))%>%
  #mutate(item=ordered(item,levels=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit")))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))

production_supply<-ggplot()+
  geom_line(data=production_per_capita_wheat_rice_maize,aes(x=year,y=value,color=item))+
  geom_line(data=supply_wheat_rice_maize,aes(x=year,y=value,color=item),linetype="dashed")+
  geom_vline(aes(xintercept = 2010),linetype="dashed")+
  annotate("text",x=1997,y=450,size=6,label="change in methodology")+
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(color=guide_legend(nrow=3, byrow=TRUE))+ 
  labs(x = "year",
       y = "grams per capita per day")

save_graphic(production_supply, "1.1.1", "global_production_supply")
save_csv(production_per_capita_wheat_rice_maize, "1.1.1", "global_production_supply")

oilseed_stuff <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/Oilseeds+other_stuff.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value,item)


production_per_capita_oilseed_stuff<-oilseed_stuff%>%
  left_join(population,by=c("year"="year"))%>%
  mutate(value=(((value.x)/value.y)*1000)/365)%>%
  mutate(item=if_else(item%in%c("Citrus Fruit, Total","Fruit Primary","Vegetables Primary"),"Fruit and Vegetables, Primary + Citrus Fruit",item))%>%
  #mutate(item=as.factor(item))%>%
  #mutate(item=ordered(item,levels=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit")))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))

production_per_capita_animal_products<-production_per_capita_oilseed_stuff%>%
  filter(item%in%c("Eggs Primary","Meat, Total","Milk, Total"))


production_per_capita_vegetal<-production_per_capita_oilseed_stuff%>%
  filter(item%in%c("Soya beans","Cereals, primary","Fruit and Vegetables, Primary + Citrus Fruit","Pulses, Total","Roots and Tubers, Total"))

production_vegetal_chart <- production_per_capita_vegetal|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = item), lwd = 1) +
  scale_x_continuous(limits = c(1959.5,2022.5),breaks =seq(1960,2022,10)) +
  scale_colour_manual(values = af_colours("categorical",n=5))+#,limits=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit"))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production g per capita per day",
       shape="",
       colour="")

save_graphic(production_vegetal_chart, "1.1.1", "global vegetal food production")
save_csv(production_per_capita_vegetal, "1.1.1", "global vegetal food production")

production_animal_products_chart <- production_per_capita_animal_products|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = item), lwd = 1) +
  scale_x_continuous(limits = c(1959.5,2022.5),breaks =seq(1960,2022,10)) +
  scale_colour_manual(values = af_colours("categorical",n=3))+#,limits=c("Cereals, primary","Eggs Primary","Meat, Total","Milk, Total","Roots and Tubers, Total","Fruit and Vegetables, Primary + Citrus Fruit"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production g per capita per day",
       shape="",
       colour="")

save_graphic(production_animal_products_chart, "1.1.1", "global animal food production")
save_csv(production_per_capita_animal_products, "1.1.1", "global animal food production")

ghg <- aws.s3::s3read_using(FUN = read_csv,
                                                    bucket = ukfsr::s3_bucket(),
                                                    object = "theme_1/t1_1_1/input/csv/ghg.csv")%>%
  select(-`Deviation from mean`)%>%
  pivot_longer(cols=2:8,names_to = "statistic",values_to = "value")



ghg_chart<-ggplot()+
  coord_flip()+
  geom_boxplot(data=ghg,outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE,aes(x= fct_reorder(Name, value),y=value,fill=food_type))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(color=guide_legend(nrow=5, byrow=TRUE))+ 
  labs(x = "",
       y = "kg CO2-eq/kg produce")

save_graphic(ghg_chart, "1.1.1", "greenhousegas")
save_csv(ghg, "1.1.1", "greenhousegas")

fbs <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/FoodBalanceSheets_E_All_Data_NOFLAG.csv")%>%
  filter(Element%in%c("Domestic supply quantity","Feed","Seed","Losses","Processing","Other uses (non-food)","Residuals","Food"))%>%
  pivot_longer(cols=10:22,names_to = "Year",values_to = "Value")%>%
  mutate(Year=as.numeric(substr(Year,2,5)))%>%
  select(-`Area Code`,-`Area Code (M49)`,-`Item Code`,-`Item Code (FBS)`,-`Element Code`)

fbs_domestic_supply_quantity<-fbs%>%
  filter(Element=="Domestic supply quantity")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  rename(domestic_supply_quantity=Value)%>%
  select(-Element)


fbs_feed<-fbs%>%
  filter(Element=="Feed")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  rename(feed=Value)%>%
  select(-Element)


fbs_seed<-fbs%>%
  filter(Element=="Seed")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  rename(seed=Value)%>%
  select(-Element)

fbs_losses<-fbs%>%
  filter(Element=="Losses")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  ungroup()%>%
  rename(losses=Value)%>%
  select(-Element)

fbs_processing<-fbs%>%
  filter(Element=="Processing")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  ungroup()%>%
  rename(processing=Value)%>%
  select(-Element)

fbs_other_uses<-fbs%>%
  filter(Element=="Other uses (non-food)")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  ungroup()%>%
  rename(other_uses=Value)%>%
  select(-Element)

fbs_residuals<-fbs%>%
  filter(Element=="Residuals")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  ungroup()%>%
  rename(residuals=Value)%>%
  select(-Element)

fbs_food<-fbs%>%
  filter(Element=="Food")%>%
  group_by(Area,Item,Element,Unit,Year)%>%
  summarise(Value=first(Value))%>%
  ungroup()%>%
  rename(food=Value)%>%
  select(-Element)

fbs_new<-fbs%>%
  group_by(Area,Item,Unit,Year)%>%
  summarise(n=n())%>%
  ungroup()%>%
  select(-n)
  
fbs_new2<-fbs_new%>%
  left_join(fbs_domestic_supply_quantity,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_feed,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_seed,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_losses,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_processing,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_other_uses,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_residuals,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  left_join(fbs_food,by=c("Year"="Year","Area"="Area","Item"="Item","Unit"="Unit"))%>%
  filter(!Item%in%c("Cereals - Excluding Beer",
                   "Starchy Roots",
                   "Sugar Crops",
                   "Sugar & Sweeteners",
                   "Pulses",
                   "Treenuts",
                   "Oilcrops",
                   "Vegetable Oils",
                   "Vegetables",
                   "Fruits - Excluding Wine",
                   "Stimulants",
                   "Spices",
                   "Alcoholic Beverages",
                   "Meat",
                   "Offals",
                   "Animal fats",
                   "Eggs",
                   "Milk - Excluding Butter",
                   "Fish, Seafood",
                   "Aquatic Products, Other"))%>%
  filter(Area=="World")

production <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/input/csv/Production_Crops_Livestock_E_All_Data_NOFLAG.csv")%>%
  filter(Element%in%c("Area harvested"))%>%
  pivot_longer(cols=10:71,names_to = "Year",values_to = "Value")%>%
  mutate(Year=as.numeric(substr(Year,2,5)))%>%
  select(-`Area Code`,-`Area Code (M49)`,-`Item Code`,-`Item Code (CPC)`,-`Element Code`)%>%
  filter(Area=="World")%>%
  rename(Area_harvested=Value)%>%
  filter(!Item%in%c("Beef and Buffalo Meat, primary",
         "Cereals, primary",
         "Citrus Fruit, Total",
         "Eggs Primary",
         "Fibre Crops, Fibre Equivalent",
         "Meat, Poultry",
         "Meat, Total",
         "Milk, Total",
         "Oilcrops, Cake Equivalent",
         "Pulses, Total",
         "Roots and Tubers, Total",
         "Sheep and Goat Meat",
         "Sugar Crops Primary",
         "Treenuts, Total",
         "Vegetables Primary"))%>%
  select(-Element)

land_area<-fbs_new2%>%
  left_join(production,by=c("Area"="Area","Item"="Item","Year"="Year"))%>%
  filter(is.na(Area_harvested))

  
