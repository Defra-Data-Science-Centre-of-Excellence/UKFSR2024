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

food_loss_percentage <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_2/input/csv/SDG12.3a.csv")
                            

food_loss_percentage_chart <- food_loss_percentage |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  rename(area=Area)|>
  mutate(region=if_else(area=="World","World","Other"))|>
  select(year,area,value,region)|>
  filter(year==2021)|>
  ggplot() +
  geom_col(aes(fct_reorder(area,value), value, fill = region), lwd = 1)+
  geom_text(aes(fct_reorder(area,value), value, color = region, label=round(value,1),hjust=1.3),size=5)+
  coord_flip()+
  theme_ukfsr()+
  #scale_y_continuous(limits = c(2000,3000)) +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = c("white","black"))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position="none")+
  labs(x = NULL,
       y = "Food Loss Percentage(%)")


save_graphic(food_loss_percentage_chart, "1.1.2", "global food loss")

household_waste_index <- aws.s3::s3read_using(FUN = read_tsv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/t1_1_2/input/csv/Household Estimates.txt")

household_waste_index_world<-household_waste_index%>%
  mutate(capita=`CONFIDENCE IN ESTIMATE`/`HOUSEHOLD ESTIMATE (TONNES/YEAR)`)%>%
  summarise(capita=sum(capita,na.rm = TRUE),household_waste=sum(`CONFIDENCE IN ESTIMATE`,na.rm = TRUE))%>%
  mutate(household_waste_per_capita=household_waste/capita)%>%
  mutate(REGION="World")%>%
  mutate(index="World")

household_waste_index_region<-household_waste_index%>%
  mutate(capita=`CONFIDENCE IN ESTIMATE`/`HOUSEHOLD ESTIMATE (TONNES/YEAR)`)%>%
  group_by(REGION)%>%
  summarise(capita=sum(capita,na.rm = TRUE),household_waste=sum(`CONFIDENCE IN ESTIMATE`,na.rm = TRUE))%>%
  mutate(household_waste_per_capita=household_waste/capita)%>%
  mutate(index="Other")#%>%
  #mutate(REGION=if_else(REGION=="Latin America and the Caribbean","Latin America and the Caribbean",REGION))%>%
  #mutate(REGION=if_else(REGION=="Australia and New Zealand","Australia and New Zealand",REGION))



household_waste_index_chart<-rbind(household_waste_index_world,household_waste_index_region)|>
  ggplot() +
  geom_col(aes(fct_reorder(REGION,household_waste_per_capita), household_waste_per_capita,fill=index), lwd = 1)+
  geom_text(aes(fct_reorder(REGION,household_waste_per_capita), household_waste_per_capita,color=index, label=round(household_waste_per_capita,1),hjust=1.3),size=5)+
  coord_flip()+
  theme_ukfsr()+
  #scale_y_continuous(limits = c(2000,3000)) +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = c("white","black"))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(plot.margin = unit(c(0.25, 0.75, 0.25, 1.25), 
                           "inches"),
        legend.position="none")+
  labs(x = NULL,
       y = "Food Loss Percentage(%)")


save_graphic(household_waste_index_chart, "1.1.2", "household waste index")
