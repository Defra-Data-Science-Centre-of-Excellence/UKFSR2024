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

# Food loss percentage ---------------------------------------------------------
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


save_graphic(food_loss_percentage_chart, "1.1.2a", "global food loss")
save_csv(food_loss_percentage, "1.1.2a", "global food loss")


# Household food waste ---------------------------------------------------------

conf_levels <- c(
  "Very low confidence","Low confidence","Medium confidence","High confidence","Eurostat"    
)


food_loss_waste <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_1_2/input/csv/FLW.csv")%>%
  mutate(`HOUSEHOLD ESTIMATE (TONNES/YEAR)`=as.numeric(`HOUSEHOLD ESTIMATE (TONNES/YEAR)`))%>%
  group_by(REGION,`CONFIDENCE IN ESTIMATE`)%>%
  summarise(household_estimates=sum(`HOUSEHOLD ESTIMATE (TONNES/YEAR)`,na.rm=TRUE))%>%
  mutate(CONFIDENCE=factor(`CONFIDENCE IN ESTIMATE`,levels =conf_levels ))

food_loss_waste_2<-food_loss_waste%>%
  group_by(REGION)%>%
  summarise(he=sum(household_estimates,na.rm=TRUE))%>%
  left_join(food_loss_waste,by=c("REGION"="REGION"))

flw_chart<-food_loss_waste_2%>%
  ggplot() +
  geom_col(aes(x=fct_reorder(REGION,he),y=household_estimates/1E6,fill=CONFIDENCE),lwd = 1)+
  geom_text(aes(x=fct_reorder(REGION,he),y=he/1E6,label=round(he/1E6,0)), hjust = -0.5, size = 6,
            position = position_dodge(width = 1),
            inherit.aes = TRUE)+
  theme_ukfsr()+
  scale_y_continuous(limits = c(0,150)) +
  coord_flip()+
  #scale_color_manual(values = af_colours("duo")) +
  scale_fill_manual(values = af_colours("categorical"),n=6) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(plot.margin=unit(c(0.2,2,0.2,0.2),"cm"))+
  annotate("text", x = 5, y = 120, label = "Note:\nregions may not include\nall countries and confidence\nin the data varies between\ncountries",size=6)+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))+ 
  labs(x = NULL,
       y = "million tonnes")

save_graphic(flw_chart, "1.1.2b", "household food waste")
save_csv(food_loss_waste_2, "1.1.2b", "household food waste")


# food waste by commodity-------------------------------------------------------
food_waste_percentages <- aws.s3::s3read_using(FUN = read_csv,
                                               bucket = ukfsr::s3_bucket(),
                                               object = "theme_1/t1_1_2/input/csv/food_waste_percentages.csv")%>%
  pivot_longer(cols=2:3,names_to = "measure",values_to="value")

food_waste_percentages_chart<-food_waste_percentages%>%
  ggplot() +
  geom_col(aes(x=measure,y=value,fill=food_type), lwd = 1)+
  geom_text(aes(x = measure, y = value, label = value, group = food_type),size=6,color="white",position = position_stack(vjust = .5))+
  theme_ukfsr()+
  #scale_y_continuous(limits = c(2000,3000)) +
  scale_color_manual(values = af_colours("duo")) +
  scale_fill_manual(values = af_colours("categorical"),n=5) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE))+ 
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")


save_graphic(food_waste_percentages_chart, "1.1.2c", "food waste percentages")
save_csv(food_waste_percentages, "1.1.2c", "food waste percentages")
