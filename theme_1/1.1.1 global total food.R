library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

population <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_1_1/input/csv/global_population_2022.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(item=Item) |>
  select(year,value)

# Vegetal production -----------------------------------------------------------

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
  mutate(item=if_else(item%in%c("Castor oil seeds","Coconuts, in shell","Groundnuts, excluding shelled","Hempseed","Kapok fruit","Karite nuts (sheanuts)","Linseed","Melonseed","Mustard seed","Oil palm fruit","Olives","Other oil seeds, n.e.c.","Poppy seed","Rape or colza seed","Safflower seed","Seed cotton, unginned","Sesame seed","Soya beans","Sunflower seed","Tallowtree seeds","Tung nuts","Jojoba seeds"),"Oilseeds, Primary",item))%>%
  group_by(year,item)%>%
  summarise(value=sum(value,na.rm=TRUE))

production_per_capita_animal_products<-production_per_capita_oilseed_stuff%>%
  filter(item%in%c("Eggs Primary","Meat, Total","Milk, Total"))


production_per_capita_vegetal<-production_per_capita_oilseed_stuff%>%
  filter(item%in%c("Oilseeds, Primary","Cereals, primary","Fruit and Vegetables, Primary + Citrus Fruit","Pulses, Total","Roots and Tubers, Total"))

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

save_graphic(production_vegetal_chart, "1.1.1a", "global vegetal food production")
save_csv(production_per_capita_vegetal, "1.1.1a", "global vegetal food production")


# Animal products --------------------------------------------------------------

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

save_graphic(production_animal_products_chart, "1.1.1b", "global animal food production")
save_csv(production_per_capita_animal_products, "1.1.1b", "global animal food production")


# Dietary energy supply --------------------------------------------------------
food_supply <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/t1_1_1/input/csv/food_supply_output.csv")

food_supply_chart<-ggplot(food_supply)+
  geom_line(aes(x=Year,y=Value),color=af_colours("duo")[1])+
  geom_vline(aes(xintercept = 2010),linetype="dashed")+
  facet_wrap(~Area)+#,scales="free")+
  #annotate("text",x=2000,y=1500,size=6,label="change in\nmethodology")+
  scale_y_continuous(limits=c(0,4000))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(panel.spacing = unit(1, "cm"),
        plot.margin=unit(c(0.2,1,0.2,0.2),"cm"))+
  labs(x = NULL,
       y = "kcals per capita per day")

save_graphic(food_supply_chart, "1.1.1d", "global food supply")
save_csv(food_supply, "1.1.1d", "global food supply")


# Biofuel production -----------------------------------------------------------
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

save_graphic(global_biofuel_production_chart, "1.1.1e", "global biofuel production")
save_csv(global_biofuel_production, "1.1.1e", "global_biofuel_production")

# Alt biofuel chart ------------------------------------------------------------
global_biofuel_production_in <- aws.s3::s3read_using(FUN = read_csv,
                                                     bucket = ukfsr::s3_bucket(),
                                                     object = "theme_1/t1_1_1/input/csv/OECD.TAD.ATM,DSD_AGR@DF_OUTLOOK_2024_2033,1.1+W.A.CPC_01802+CPC_216+CPC_0112....csv")%>%
  filter(Measure%in%c("Production","Biofuel use"))

global_biofuel_production<-global_biofuel_production_in%>%
                          filter(Measure%in%c("Production"))%>%
  select(TIME_PERIOD,Commodity,OBS_VALUE)

global_biofuel_use<-global_biofuel_production_in%>%
  filter(Measure%in%c("Biofuel use"))%>%
  select(TIME_PERIOD,Commodity,OBS_VALUE)

global_biofuel_production<-global_biofuel_production%>%
  left_join(global_biofuel_use,c("TIME_PERIOD"="TIME_PERIOD","Commodity"="Commodity"))%>%
  mutate(percentage=(OBS_VALUE.y/OBS_VALUE.x)*100)%>%
  filter(TIME_PERIOD<2025)

global_biofuel_production_chart <- ggplot(data=global_biofuel_production) +
  geom_line(aes(x=TIME_PERIOD ,y=percentage,color=Commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(breaks = seq(2000,2024,2))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Biofuel demand share of\nglobal crop production")

save_graphic(global_biofuel_production_chart, "1.1.1e", "global biofuel production")
save_csv(global_biofuel_production, "1.1.1e", "global_biofuel_production")

# Annual growth in demand for key products--------------------------------------

average_annual_growth_in_demand_for_key_commodity_groups <- aws.s3::s3read_using(FUN = read_csv,
                                                                                 bucket = ukfsr::s3_bucket(),
                                                                                 object = "theme_1/t1_1_1/input/csv/Average_annual_growth_in_demand_for_key_commodity_groups_2013-22_and_2023-32.csv")




average_annual_growth_in_demand_for_key_commodity_groups <- average_annual_growth_in_demand_for_key_commodity_groups |> 
  rename(year=Year) |>
  rename(commodity=Commodity) |>
  mutate(commodity_year=paste0(commodity,"\n",year)) |>
  pivot_longer(3:4,values_to = "value",names_to = "growth_type")|>
  select(year,commodity,growth_type,value)

average_annual_growth_in_demand_for_key_commodity_groups_chart <- average_annual_growth_in_demand_for_key_commodity_groups |> 
  ggplot() +
  geom_col(aes(x=year,y=value,fill=growth_type))+
  facet_wrap(~commodity,scales="free")+
  theme_ukfsr()+
  scale_y_continuous(limits = c(0,2)) +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = c("white","black"))+ 
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE) +
  labs(x = NULL,
       y = "percent per annum")


save_graphic(average_annual_growth_in_demand_for_key_commodity_groups_chart, "1.1.1f", "average annual growth in demand for key commodity groups")
save_csv(average_annual_growth_in_demand_for_key_commodity_groups, "1.1.1f", "average annual growth in demand for key commodity groups")
