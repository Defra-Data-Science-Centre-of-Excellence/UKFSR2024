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

