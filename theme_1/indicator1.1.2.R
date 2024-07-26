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
                            object = "theme_1/t1_1_2/input/csv/SDG12.3a.csv")%>%
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  rename(area=Area)|>
  mutate(region=if_else(area=="World","World","Other"))|>
  select(year,area,value,region)|>
  filter(year==2021)
                            

food_loss_percentage_chart <- food_loss_percentage |> 
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
save_csv(food_loss_percentage, "1.1.2", "global food loss")

household_waste_index <- aws.s3::s3read_using(FUN = read_csv,
                                             bucket = ukfsr::s3_bucket(),
                                             object = "theme_1/t1_1_2/input/csv/Household Estimate.csv")%>%
  mutate(index=if_else(Region=="World","1","0"))%>%
  mutate(Average=as.numeric(Average))%>%
  filter(!is.na(Average))%>%
  rename(region=Region)%>%
  rename(average=Average)%>%
  select(region,average,index)





household_waste_index_chart<-household_waste_index%>%#rbind(household_waste_index_world,household_waste_index_region)|>
  ggplot() +
  geom_col(aes(fct_reorder(region,average), average,fill=index), lwd = 1)+
  geom_text(aes(fct_reorder(region,average), average,color=index, label=round(average,1),hjust=1.3),size=5)+
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
       y = "Household Food Waste(kg/capita/year)")


save_graphic(household_waste_index_chart, "1.1.2", "household waste index")
save_csv(household_waste_index, "1.1.2", "household waste index")
