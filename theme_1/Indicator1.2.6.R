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
library(scales)
library(zoo)

land_degradation <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/t1_2_6/input/csv/land_degradation.csv")

land_degradation_chart<-ggplot()+
  geom_col(data=land_degradation,aes(x=region,y=percentage,fill=status),position="stack")+
  coord_flip()+
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent") 

save_graphic(land_degradation_chart, "1.2.6", "land degradation chart")
save_csv(land_degradation, "1.2.6", "land degradation")


sdg_15_3_1 <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_2_6/input/csv/sdg15_3_1.csv")%>%
  pivot_longer(cols=2:3,names_to = "year",values_to ="value")%>%
  mutate(position=if_else(year==2015,value-1,value+1))
  
sdg_15_3_1_chart<-ggplot()+
  geom_line(data=sdg_15_3_1,aes(x=Region,y=value,group=Region))+
  geom_point(data=sdg_15_3_1,aes(x=Region,y=value,color=year),size=4)+
  geom_text(data=sdg_15_3_1,aes(x=Region,y=position,label=value),size=6)+
  annotate("text", x = 11, y = 24, label = "Note\nregions and sub regions\nmay not include\nall countries",size=6)+
  coord_flip()+
  scale_color_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website",horizontal = TRUE) +
  labs(x = NULL,
       y = "percent") 

save_graphic(sdg_15_3_1_chart, "1.2.6", "sdg_15_3_1_chart")
save_csv(sdg_15_3_1, "1.2.6", "sdg_15_3_1")
