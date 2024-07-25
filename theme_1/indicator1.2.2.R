### Data
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(readxl)

aquastat <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_2/input/csv/aquastat.csv")

aquastat_world<-aquastat%>%
  filter(Country=="World")

aquastat_regions<-aquastat%>%
  filter(Country%in%c("World","Northern Africa and Western Asia","Latin America and the Caribbean","Europe and Northern America","Sub-Saharan Africa","Eastern and South-Eastern Asia","Central and Southern Asia","Australia and New Zealand","Oceania (excluding Australia and New Zealand)"))


agricultural_water_withdrawal<-aquastat_regions%>%
  filter(Variable=="Agricultural water withdrawal")

percentage_cultivated_area_irrigation<-aquastat_regions%>%
  filter(Variable=="% of the cultivated area equipped for irrigation")

percentage_cultivated_area_irrigation<-aquastat_regions%>%
  filter(Variable=="% of the cultivated area equipped for irrigation")

agricultural_water_withdrawal_percentage<-aquastat_regions%>%
  filter(Variable=="Agricultural water withdrawal as % of total renewable water resources")

water_stress<-aquastat_regions%>%
  filter(Variable=="SDG 6.4.2. Water Stress")


########

agricultural_water_withdrawal_chart<-ggplot()+
  geom_line(data=agricultural_water_withdrawal,aes(x = Year, y = Value, colour = Country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "10^9_M3_YEAR")

save_graphic(agricultural_water_withdrawal_chart, "1.2.2", "agricultural_water_withdrawal")
save_csv(agricultural_water_withdrawal, "1.2.2", "agricultural_water_withdrawal")


percentage_cultivated_area_irrigation_chart<-ggplot()+
  geom_line(data=percentage_cultivated_area_irrigation,aes(x = Year, y = Value, colour = Country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  #theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(percentage_cultivated_area_irrigation_chart, "1.2.2", "percentage cultivated area irrigation")
save_csv(percentage_cultivated_area_irrigation, "1.2.2", "percentage cultivated area irrigation")

agricultural_water_withdrawal_percentage_chart<-ggplot()+
  geom_line(data=agricultural_water_withdrawal_percentage,aes(x = Year, y = Value, colour = Country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(agricultural_water_withdrawal_percentage_chart, "1.2.2", "agricultural water withdrawal percentage")
save_csv(agricultural_water_withdrawal_percentage, "1.2.2", "agricultural water withdrawal percentage")

water_stress_chart<-ggplot()+
  geom_line(data=water_stress,aes(x = Year, y = Value, colour = Country)) +
  #scale_y_continuous(limits = c(0,50)) +
  #scale_x_continuous(limits = c(2014,2023),breaks=seq(2015,2023,2),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  #scale_colour_manual(values = af_colours("categorical",n=3)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(water_stress_chart, "1.2.2", "water stress")
save_csv(water_stress, "1.2.2", "water stress")
