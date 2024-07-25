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



