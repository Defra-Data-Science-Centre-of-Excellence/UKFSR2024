### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(stringi)
library(stringr)
library(readr)
library(ukfsr)
library(readxl)
library(afcolours)
library(here)
library(lubridate)


#FAOSTAT

source(here("utils", "load-font.R"))

phoma_canker <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/t2_1_6/input/phoma_canker.csv")%>%
  pivot_longer(cols=3:5,names_to="measure",values_to="value")

phoma_canker_chart <-phoma_canker |> 
  ggplot() +
  geom_line(aes(x=survey_year,y=value/100,color=measure))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("categorical",n=3))+
  guides(color=guide_legend(nrow=3,byrow=TRUE))+
  scale_x_continuous(breaks=seq(2003,2023,2),labels = seq(2003,2023,2))+
  scale_y_continuous(labels = scales::percent)+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "")

save_graphic(phoma_canker_chart, "2.1.6", "phoma canker chart")
save_csv(phoma_canker, "2.1.6", "phoma_canker")

sea_lice <- aws.s3::s3read_using(FUN = read_csv,
                                     bucket = ukfsr::s3_bucket(),
                                     object = "theme_2/t2_1_6/input/nat_mort.csv")%>%
  mutate(month=month(date))%>%
  select(year,month,mortality)%>%
  filter(year!=2018)%>%
  mutate(year=as.character(year))

sea_lice_chart <-sea_lice |> 
  ggplot() +
  geom_line(aes(x=month,y=mortality,color=year))+
  theme_ukfsr()+
  scale_color_manual(values = af_colours("categorical",n=6))+
  scale_x_continuous(breaks=seq(1,12,1),labels = seq(1,12,1))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(sea_lice_chart, "2.1.6", "sea lice chart")
save_csv(sea_lice, "2.1.6", "sea_lice")
