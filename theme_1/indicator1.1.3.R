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

average_annual_growth_in_demand_for_key_commodity_groups <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_3/input/csv/Average_annual_growth_in_demand_for_key_commodity_groups_2013-22_and_2023-32.csv")
                            



average_annual_growth_in_demand_for_key_commodity_groups <- average_annual_growth_in_demand_for_key_commodity_groups |> 
  rename(year=Year) |>
  rename(commodity=Commodity) |>
  mutate(commodity_year=paste0(commodity,"\n",year)) |>
  pivot_longer(3:4,values_to = "value",names_to = "growth_type")|>
  select(year,commodity,growth_type,value)

average_annual_growth_in_demand_for_key_commodity_groups_chart <- average_annual_growth_in_demand_for_key_commodity_groups |> 
  ggplot() +
  geom_col(aes(x=year,y=value,fill=growth_type))+
  facet_wrap(~commodity)+
  theme_ukfsr()+
  #scale_y_continuous(limits = c(2000,3000)) +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = c("white","black"))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent per annum")


save_graphic(average_annual_growth_in_demand_for_key_commodity_groups_chart, "1.1.3", "average annual growth in demand for key commodity groups")
save_csv(average_annual_growth_in_demand_for_key_commodity_groups, "1.1.3", "average annual growth in demand for key commodity groups")


grfc2024 <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/t1_1_3/input/csv/GRFC2024_Master.csv")
grfc2024_in<-grfc2024%>%
  mutate(`Total country population`=as.numeric(`Total country population`,na.rm=FALSE))%>%
  mutate(`Population analysed`=as.numeric(`Population analysed`,na.rm=FALSE))%>%
  mutate(`Population in Phase 3 or above #`=as.numeric(`Population in Phase 3 or above #`,na.rm=FALSE))%>%
  mutate(`Population in Phase 1 #`=as.numeric(`Population in Phase 1 #`,na.rm=FALSE))%>%  
  mutate(`Population in Phase 2 #`=as.numeric(`Population in Phase 2 #`,na.rm=FALSE))%>% 
  group_by(`Year of reference`)%>%
  summarise(total_pop=sum(`Total country population`,na.rm=TRUE),pop_analysed=sum(`Population analysed`,na.rm=TRUE),pop_3_plus=sum(`Population in Phase 3 or above #`,na.rm=TRUE),pop_1=sum(`Population in Phase 1 #`,na.rm=TRUE),pop_2=sum(`Population in Phase 2 #`,na.rm=TRUE))%>%
  mutate(pop_1_plus_2=pop_1+pop_2)%>%
  mutate(rem_pop=total_pop-pop_1_plus_2-pop_3_plus)%>%
  mutate(pop_3_plus_per=round(((pop_3_plus/pop_analysed)*100),1))%>%
  rename(year=`Year of reference`)%>%
  select(year,pop_1_plus_2,pop_3_plus,rem_pop,pop_3_plus_per)%>%
  pivot_longer(2:5,values_to = "value",names_to = "category")

grfc2024_out<-grfc2024_in%>%
  filter(category!="pop_3_plus_per")%>%
  mutate(category=factor(category,levels=c("rem_pop","pop_1_plus_2","pop_3_plus")))

grfc2024_chart<-grfc2024_out%>%
  ggplot()+
  geom_col(aes(x=year,y=value/1e6,fill=category))+
  theme_ukfsr()+
  scale_fill_manual(values = af_colours("sequential"),labels=c("rest of population","1+2 No/Minimal+Stressed","3 Crisis")) +
  #scale_color_manual(values = c("white","black"))+ 
  scale_x_continuous(breaks=seq(2016,2023,1))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Millions")

save_graphic(grfc2024_chart, "1.1.3", "numbers of people and share of analysed population in GRFC countries-territories 
facing high levels of acute food insecurity, 2016–2023 ")
save_csv(grfc2024_out, "1.1.3", "numbers of people and share of analysed population in GRFC countries-territories 
facing high levels of acute food insecurity, 2016–2023")

number_of_moderately_or_severely_food_insecure_people <- aws.s3::s3read_using(FUN = read_csv,
                                                                              bucket = ukfsr::s3_bucket(),
                                                                              object = "theme_1/t1_1_3/input/csv/Number of moderately or severely food insecure people.csv")

number_of_moderately_or_severely_food_insecure_people_world<-number_of_moderately_or_severely_food_insecure_people%>%
  mutate(Value=as.numeric(Value))%>%
  filter(Area=="World")%>%
  filter(Item=="Number of moderately or severely food insecure people (million) (annual value)")%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,value)
  

number_of_moderately_or_severely_food_insecure_people_world_chart<-number_of_moderately_or_severely_food_insecure_people_world%>%
  ggplot()+
  geom_col(aes(x=year,y=value))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_color_manual(values = af_colours("duo")) +
  scale_fill_manual(values = af_colours("duo")) +
  #scale_x_continuous(breaks=seq(2016,2023,1))+
  labs(x = NULL,
       y = "Millions")

save_graphic(number_of_moderately_or_severely_food_insecure_people_world_chart, "1.1.3", "Number of moderately or severely food insecure people")
save_csv(number_of_moderately_or_severely_food_insecure_people_world, "1.1.3", "Number of moderately or severely food insecure people")

coahd <- aws.s3::s3read_using(FUN = read_csv,
                                                                              bucket = ukfsr::s3_bucket(),
                                                                              object = "theme_1/t1_1_3/input/csv/CoAHD.csv")%>%
  filter(!Area=="Europe")%>%
  rename(area="Area")%>%
  rename(year="Year")%>%
  rename(value="Value")%>%
  select(year,area,value)

coahd_chart<-coahd%>%
  ggplot()+
  geom_line(aes(x=year,y=value,group=area,color=area))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_color_manual(values = af_colours("categorical",n=6)) +
  #scale_x_continuous(breaks=seq(2016,2023,1))+
  labs(x = NULL,
       y = "percent")

save_graphic(coahd_chart, "1.1.3", "coahd")
save_csv(coahd, "1.1.3", "coahd")

world_data<-map_data("world")

food_supply_2022 <- aws.s3::s3read_using(FUN = read_csv,
                                                                              bucket = ukfsr::s3_bucket(),
                                                                              object = "theme_1/t1_1_3/input/csv/foodsupply2022.csv")

food_supply_2022_key<-food_supply_2022%>%
  mutate(gpcpd=(Value*1000)/365)%>%
  mutate(Item=if_else(Item%in%c("Pigmeat","Mutton & Goat Meat","Bovine Meat","Meat, Other"),"Red Meat",Item))%>%
  group_by(Area,Item)%>%
  summarise(gpcpd=sum(gpcpd,na.rm = TRUE))%>%
  mutate(key="a")%>%
  mutate(key=if_else(Item=="Cereals - Excluding Beer",if_else(gpcpd<232,"i",if_else(gpcpd<308.6,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Starchy Roots",if_else(gpcpd<50,"i",if_else(gpcpd<66.5,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Sugar & Sweeteners",if_else(gpcpd<31,"i",if_else(gpcpd<41.2,"ii","iii")),key))%>%           
  mutate(key=if_else(Item=="Pulses",if_else(gpcpd<75,"i",if_else(gpcpd<98.8,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Treenuts",if_else(gpcpd<50,"i",if_else(gpcpd<66.5,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Vegetables",if_else(gpcpd<300,"i",if_else(gpcpd<399,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Fruits - Excluding Wine",if_else(gpcpd<200,"i",if_else(gpcpd<266,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Poultry Meat",if_else(gpcpd<29,"i",if_else(gpcpd<38.6,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Red Meat",if_else(gpcpd<14,"i",if_else(gpcpd<18.6,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Eggs",if_else(gpcpd<13,"i",if_else(gpcpd<17.3,"ii","iii")),key))%>%
  mutate(key=if_else(Item=="Milk - Excluding Butter",if_else(gpcpd<250,"i",if_else(gpcpd<332.5,"ii","iii")),key))%>%        
  mutate(key=if_else(Item=="Fish, Seafood",if_else(gpcpd<28,"i",if_else(gpcpd<37.3,"ii","iii")),key))%>%
  mutate(Item=if_else(Item=="Cereals - Excluding Beer","Cereals",Item))%>%
  mutate(Item=if_else(Item=="Starchy Roots","Roots and Tubers",Item))%>%
  mutate(Item=if_else(Item=="Sugar & Sweeteners","Sugar",Item))%>%           
  mutate(Item=if_else(Item=="Pulses","Legumes",Item))%>%
  mutate(Item=if_else(Item=="Treenuts","Nuts",Item))%>%
  mutate(Item=if_else(Item=="Fruits - Excluding Wine","Fruits",Item))%>%
  mutate(Item=if_else(Item=="Milk - Excluding Butter","Milk",Item))%>%        
  mutate(Item=if_else(Item=="Fish, Seafood","Seafood",Item))


food_supply_2022_key<-food_supply_2022_key%>%
  mutate(Area=if_else(Area=="United States of America","USA",Area))%>%
  mutate(Area=if_else(Area=="Syrian Arab Republic","Syria",Area))%>%
  mutate(Area=if_else(Area=="Netherlands (Kingdom of the)","Netherlands",Area))%>%
  mutate(Area=if_else(Area=="Viet Nam","Vietnam",Area))%>%
  mutate(Area=if_else(Area=="Brunei Darussalam","Brunei",Area))%>%
  mutate(Area=if_else(Area=="Venezuela (Bolivarian Republic of)","Venezuela",Area))%>%
  mutate(Area=if_else(Area=="Türkiye","Turkey",Area))%>%
  mutate(Area=if_else(Area=="Antigua and Barbuda","Antigua",Area))%>%
  mutate(Area=if_else(Area=="Republic of Korea","South Korea",Area))%>%
  mutate(Area=if_else(Area=="Democratic People's Republic of Korea","North Korea",Area))%>%
  mutate(Area=if_else(Area=="Saint Kitts and Nevis","Saint Kitts",Area))%>%
  mutate(Area=if_else(Area=="Republic of Moldova","Moldova",Area))%>%
  mutate(Area=if_else(Area=="Russian Federation","Russia",Area))%>%
  mutate(Area=if_else(Area=="Saint Vincent and the Grenadines","Grenadines",Area))%>%
  mutate(Area=if_else(Area=="Côte d'Ivoire","Ivory Coast",Area))%>%
  mutate(Area=if_else(Area=="Trinidad and Tobago","Trinidad",Area))%>%
  mutate(Area=if_else(Area=="Czechia","Czech Republic",Area))%>%
  mutate(Area=if_else(Area=="Iran (Islamic Republic of)","Iran",Area))%>%
  mutate(Area=if_else(Area=="Bolivia (Plurinational State of)","Bolivia",Area))%>%
  mutate(Area=if_else(Area=="Cabo Verde","Cape Verde",Area))%>%
  mutate(Area=if_else(Area=="Lao People's Democratic Republic","Laos",Area))%>%
  mutate(Area=if_else(Area=="United Republic of Tanzania","Tanzania",Area))%>%
  mutate(Area=if_else(Area=="United Kingdom of Great Britain and Northern Ireland","UK",Area))%>%
  mutate(Area=if_else(Area=="Congo","Republic of Congo",Area))%>%
  mutate(Area=if_else(Area=="Eswatini","Swaziland",Area))

world_map_food_supply_2022<-world_data%>%
  left_join(food_supply_2022_key,by=c("region"="Area"))%>%
  filter(!is.na(Item))

world_map_food_supply_2022_chart<-ggplot()+
  facet_wrap(~Item)+
  geom_polygon(data = world_map_food_supply_2022,aes(x=long,y=lat,group=group,fill=key))+
  scale_fill_manual(values = c("red","blue","green")) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(world_map_food_supply_2022_chart, "1.1.3", "food supply")
save_csv(food_supply_2022_key, "1.1.3", "food supply.csv")

food_supply_2019_2022 <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_3/input/csv/food_supply_2019-2022.csv")

food_supply_2019<-food_supply_2019_2022%>%
  filter(Year==2019)%>%
  select(Area,Item,Value)

food_supply_2022a<-food_supply_2019_2022%>%
  filter(Year==2022)%>%
  select(Area,Item,Value)

food_supply_diff<-food_supply_2022a%>%
  left_join(food_supply_2019,by=c("Area"="Area","Item"="Item"))%>%
  mutate(Item=if_else(Item%in%c("Pigmeat","Mutton & Goat Meat","Bovine Meat","Meat, Other"),"Red Meat",Item))%>%
  group_by(Area,Item)%>%
  summarise(Value.x=sum(Value.x,na.rm = TRUE),Value.y=sum(Value.y,na.rm=TRUE))%>%
  mutate(difference=Value.x-Value.y)%>%
  mutate(percentage=(difference/Value.y)*100)%>%
  mutate(Item=if_else(Item=="Cereals - Excluding Beer","Cereals",Item))%>%
  mutate(Item=if_else(Item=="Starchy Roots","Roots and Tubers",Item))%>%
  mutate(Item=if_else(Item=="Sugar & Sweeteners","Sugar",Item))%>%           
  mutate(Item=if_else(Item=="Pulses","Legumes",Item))%>%
  mutate(Item=if_else(Item=="Treenuts","Nuts",Item))%>%
  mutate(Item=if_else(Item=="Fruits - Excluding Wine","Fruits",Item))%>%
  mutate(Item=if_else(Item=="Milk - Excluding Butter","Milk",Item))%>%        
  mutate(Item=if_else(Item=="Fish, Seafood","Seafood",Item))%>%
  mutate(key=if_else(difference>0,"positive",if_else(difference<0,"negative","no change")))

food_supply_diff<-food_supply_diff%>%
  mutate(Area=if_else(Area=="United States of America","USA",Area))%>%
  mutate(Area=if_else(Area=="Syrian Arab Republic","Syria",Area))%>%
  mutate(Area=if_else(Area=="Netherlands (Kingdom of the)","Netherlands",Area))%>%
  mutate(Area=if_else(Area=="Viet Nam","Vietnam",Area))%>%
  mutate(Area=if_else(Area=="Brunei Darussalam","Brunei",Area))%>%
  mutate(Area=if_else(Area=="Venezuela (Bolivarian Republic of)","Venezuela",Area))%>%
  mutate(Area=if_else(Area=="Türkiye","Turkey",Area))%>%
  mutate(Area=if_else(Area=="Antigua and Barbuda","Antigua",Area))%>%
  mutate(Area=if_else(Area=="Republic of Korea","South Korea",Area))%>%
  mutate(Area=if_else(Area=="Democratic People's Republic of Korea","North Korea",Area))%>%
  mutate(Area=if_else(Area=="Saint Kitts and Nevis","Saint Kitts",Area))%>%
  mutate(Area=if_else(Area=="Republic of Moldova","Moldova",Area))%>%
  mutate(Area=if_else(Area=="Russian Federation","Russia",Area))%>%
  mutate(Area=if_else(Area=="Saint Vincent and the Grenadines","Grenadines",Area))%>%
  mutate(Area=if_else(Area=="Côte d'Ivoire","Ivory Coast",Area))%>%
  mutate(Area=if_else(Area=="Trinidad and Tobago","Trinidad",Area))%>%
  mutate(Area=if_else(Area=="Czechia","Czech Republic",Area))%>%
  mutate(Area=if_else(Area=="Iran (Islamic Republic of)","Iran",Area))%>%
  mutate(Area=if_else(Area=="Bolivia (Plurinational State of)","Bolivia",Area))%>%
  mutate(Area=if_else(Area=="Cabo Verde","Cape Verde",Area))%>%
  mutate(Area=if_else(Area=="Lao People's Democratic Republic","Laos",Area))%>%
  mutate(Area=if_else(Area=="United Republic of Tanzania","Tanzania",Area))%>%
  mutate(Area=if_else(Area=="United Kingdom of Great Britain and Northern Ireland","UK",Area))%>%
  mutate(Area=if_else(Area=="Congo","Republic of Congo",Area))%>%
  mutate(Area=if_else(Area=="Eswatini","Swaziland",Area))

world_map_food_supply_diff<-world_data%>%
  left_join(food_supply_diff,by=c("region"="Area"))%>%
  filter(!is.na(Item))

world_map_food_supply_diff_chart<-ggplot()+
  facet_wrap(~Item)+
  geom_polygon(data = world_map_food_supply_diff,aes(x=long,y=lat,group=group,fill=key))+
  scale_fill_manual(values = c("red","blue","green")) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(world_map_food_supply_diff_chart, "1.1.3", "food supply difference")
save_csv(food_supply_diff, "1.1.3", "food supply difference.csv")
