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

source(here("utils", "load-font.R"))

# NOT USED regional land degragation -------------------------------------------
land_degradation <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/input_data/t1_5_1_old/land_degradation.csv")

land_degradation_chart<-ggplot()+
  geom_col(data=land_degradation,aes(x=region,y=percentage/100,fill=status),position="stack")+
  coord_flip()+
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  scale_y_continuous(labels=scales::percent)+
  labs(x = NULL,
       y = "") 

#save_graphic(land_degradation_chart, "1.2.6", "land degradation chart")
#save_csv(land_degradation, "1.2.6", "land degradation")

# % of land that is degraded ---------------------------------------------------
sdg_15_3_1 <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/input_data/t1_5_1/sdg15_3_1.csv")%>%
  pivot_longer(cols=2:3,names_to = "year",values_to ="value")%>%
  mutate(position=if_else(year==2015,value-1,value+1))
  
sdg_15_3_1_chart<-ggplot()+
  geom_line(data=sdg_15_3_1,aes(x=Region,y=value/100,group=Region))+
  geom_point(data=sdg_15_3_1,aes(x=Region,y=value/100,color=year),size=4)+
  geom_text(data=sdg_15_3_1,aes(x=Region,y=position/100,label=value),size=6)+
  annotate("text", x = 11, y = 24/100, label = "Note\nregions and sub regions\nmay not include\nall countries",size=6)+
  coord_flip()+
  scale_color_manual(values = af_colours("duo")) +
  scale_y_continuous(breaks=c(0.05,0.1,0.15,0.2,0.25),labels=scales::percent)+
  theme_ukfsr(base_family = "GDS Transport Website",horizontal = TRUE) +
  labs(x = NULL,
       y = "") 

save_graphic(sdg_15_3_1_chart, "1.5.1a", "pct land degraded sdg 15 3 1")
save_csv(sdg_15_3_1, "1.5.1a", "pct land degraded sdg 15 3 1")
