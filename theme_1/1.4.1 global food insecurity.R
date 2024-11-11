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

# People facing high insecurity GRFC countries----------------------------------
grfc2024 <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/input_data/t1_4_1/GRFC2024_Master.csv")

grfc2024_in2 <- aws.s3::s3read_using(FUN = read_csv,
                                     bucket = ukfsr::s3_bucket(),
                                     object = "theme_1/input_data/t1_4_1/grfc_number_of_countries.csv") 

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
  scale_fill_manual(values = af_colours("categorical",n=3),labels=c("rest of population","1+2 No/Minimal+Stressed","3+ Crisis")) +
  #scale_color_manual(values = c("white","black"))+ 
  scale_x_continuous(breaks=seq(2016,2023,1))+
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) +
  labs(x = NULL,
       y = "Million people")

save_graphic(grfc2024_chart, "1.4.1c", "numbers of people facing high food insecurity")
save_csv(grfc2024_out, "1.4.1c", "numbers of people facing high food insecurity")


# NOT USED alternate GRFC chart ------------------------------------------------
grfc2024_out2<-grfc2024_out%>%
  group_by(year)%>%
  summarise(value=sum(value,sum.na=TRUE))%>%
  left_join(grfc2024_in2,by=c("year"="year"))%>%
  left_join(grfc2024_out,by=c("year"="year"))


grfc2024_chart_2<-grfc2024_out2%>%
  ggplot()+
  geom_col(aes(x=year,y=value.y/1e6,fill=category))+
  geom_line(aes(x=year,y=percentage*20))+
  geom_text(aes(x=year,y=value.x/1E6+200,label=paste0("In ",`number of countries`,"\n countries")),size=7)+
  theme_ukfsr()+
  scale_fill_manual(values = af_colours("categorical",n=3),labels=c("rest of population","1+2 No/Minimal+Stressed","3+ Crisis")) +
  #scale_color_manual(values = c("white","black"))+ 
  scale_x_continuous(breaks=seq(2016,2023,1))+
  scale_y_continuous(limits=c(0,2200),sec.axis = sec_axis( ~ . / 20, name="percentage"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million people")

#save_graphic(grfc2024_chart_2, "1.1.3", "numbers of people and share of analysed population in GRFC countries-territories 
#facing high levels of acute food insecurity, 2016–2023 2")


# Number of moderate or severely food insecure people --------------------------
number_of_moderately_or_severely_food_insecure_people <- aws.s3::s3read_using(FUN = read_csv,
                                                                              bucket = ukfsr::s3_bucket(),
                                                                              object = "theme_1/input_data/t1_4_1/Number of moderately or severely food insecure people.csv")

number_of_moderately_or_severely_food_insecure_people_world<-number_of_moderately_or_severely_food_insecure_people%>%
  mutate(Value=as.numeric(Value))%>%
  filter(Area=="World")%>%
  filter(Item=="Number of moderately or severely food insecure people (million) (annual value)")%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  select(year,value)
  

number_of_moderately_or_severely_food_insecure_people_world_chart<-number_of_moderately_or_severely_food_insecure_people_world%>%
  ggplot()+
  geom_col(aes(x=year,y=value),fill=af_colours("duo")[1])+
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, 0.05)))+
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) +
  #scale_x_continuous(breaks=seq(2016,2023,1))+
  labs(x = NULL,
       y = "Million people")


save_graphic(number_of_moderately_or_severely_food_insecure_people_world_chart, "1.4.1b", "Number of moderately or severely food insecure people")
save_csv(number_of_moderately_or_severely_food_insecure_people_world, "1.4.1b", "Number of moderately or severely food insecure people")

# number_of_moderately_or_severely_food_insecure_people_world_chart<-number_of_moderately_or_severely_food_insecure_people_world%>%
#   ggplot()+
#   geom_col(aes(x=year,y=value),fill=af_colours("duo")[1])+
#   geom_text(aes(x=year,y=value,label=round(household_estimates/1E6,0)), hjust = -0.5, size = 6,position = position_dodge(width = 1),inherit.aes = TRUE)+
#   theme_ukfsr(base_family = "GDS Transport Website") +
#   #scale_x_continuous(breaks=seq(2016,2023,1))+
#   labs(x = NULL,
#        y = "Millions")

# Percentage of the population unable to afford a healthy diet------------------

coahd <- aws.s3::s3read_using(FUN = read_csv,
                              bucket = ukfsr::s3_bucket(),
                              object = "theme_1/input_data/t1_4_1/CoAHD.csv")%>%
  filter(!Area=="Europe")%>%
  rename(area="Area")%>%
  rename(year="Year")%>%
  rename(value="Value")%>%
  select(year,area,value)

coahd_chart<-coahd%>%
  ggplot()+
  geom_line(aes(x=year,y=value/100,group=area,color=area))+
  geom_point(aes(x=year,y=value/100,shape=area,fill=area),size=4)+
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_color_manual(values = c("#A285D1","#28A197","#801650","#F46A25","#3D3D3D","#12436D")) +
  scale_fill_manual(values = c("#A285D1","#28A197","#801650","#F46A25","#3D3D3D","#12436D")) +
  scale_shape_manual(values = c(NA,NA,NA,NA,NA,24)) +
  guides(color=guide_legend(nrow=3, byrow=TRUE))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = NULL,
       y = "")

save_graphic(coahd_chart, "1.4.1d", "pct population unable to afford a healthy diet")
save_csv(coahd, "1.4.1d", "pct population unable to afford a healthy diet")


# NOT USED World map food supply 2022-----------------------------------------------

world_data<-map_data("world")

food_supply_2022 <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/input_data/t1_4_1_old/foodsupply2022.csv")

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

# save_graphic(world_map_food_supply_2022_chart, "1.1.3", "food supply")
# save_csv(food_supply_2022_key, "1.1.3", "food supply.csv")


# NOT USED World map food supply difference ------------------------------------

food_supply_2019_2022 <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/input_data/t1_4_1_old/food_supply_2019-2022.csv")

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

# save_graphic(world_map_food_supply_diff_chart, "1.1.3", "food supply difference")
# save_csv(food_supply_diff, "1.1.3", "food supply difference.csv")

# NOT USED prevalence of undernourished people ---------------------------------
prevalance_of_undernourised <- aws.s3::s3read_using(FUN = read_csv,
                                              bucket = ukfsr::s3_bucket(),
                                              object = "theme_1/input_data/t1_4_1_old/prevalance_of_undernourised.csv")%>%
  filter(`Year Code`<2025)%>%
  mutate(name="prevalence of undernourised")

prevalance_of_undernourised_chart<-prevalance_of_undernourised%>%
  ggplot()+
  geom_line(aes(x=`Year Code`,y=Value/100,color=Area,group=Area))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_color_manual(values = af_colours("duo")) +
  scale_y_continuous(labels=scales::percent)+
  theme(legend.position = "None")+
  labs(x = NULL,
       y = "")

# save_graphic(prevalance_of_undernourised_chart, "1.1.3", "prevalance_of_undernourised")
# save_csv(prevalance_of_undernourised, "1.1.3", "prevalance_of_undernourised.csv")


# Number of undernourished people ---------------------------------------------- 
number_of_undernourised <- aws.s3::s3read_using(FUN = read_csv,
                                              bucket = ukfsr::s3_bucket(),
                                              object = "theme_1/input_data/t1_4_1/number_of_people_undernourised.csv")%>%
  filter(`Year Code`<2025)%>%
  mutate(name="number of undernourised")

number_of_undernourised_chart<-ggplot()+
  geom_col(data=number_of_undernourised,aes(x=`Year Code`,y=Value,group=Area,fill=Area))+
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = af_colours("duo")[2]) +
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0, 0.05)))+
  #scale_x_continuous(breaks=seq(2016,2023,1))+
  labs(x = NULL,
       y = "Million people") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "none")


save_graphic(number_of_undernourised_chart, "1.4.1a", "number of undernourished people")
save_csv(number_of_undernourised, "1.4.1a", "number of undernourished people")


# NOT USED prevalence/number of undernourished people ---------------------------------
prevalance_number_of_undernourised_chart<-ggplot()+
  geom_col(data=number_of_undernourised,aes(x=`Year Code`,y=Value,group=name,fill=name))+
  geom_line(data=prevalance_of_undernourised,aes(x=`Year Code`,y=Value*80,group=name,color=name))+
  scale_y_continuous(sec.axis = sec_axis(~ . * 4.80/100, name = "prevalence"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = af_colours("duo")[2]) +
  #scale_x_continuous(breaks=seq(2016,2023,1))+
  labs(x = NULL,
       y = "Million people")

# save_graphic(prevalance_number_of_undernourised_chart, "1.1.3", "prevalance number_of_people_undernourised")

