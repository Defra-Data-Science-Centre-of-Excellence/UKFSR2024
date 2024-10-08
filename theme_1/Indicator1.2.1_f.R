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
library(tidyverse)

tfp_out_2017_2021 <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_2_1/input/csv/agricultural_productivity.csv")


tfp_out_2017_2021_chart<-ggplot()+
  geom_col(data=tfp_out_2017_2021,aes(x = year, y = growth,fill=year)) +
  facet_wrap(~`income group`,scales='free')+
  scale_y_continuous(limits=c(0,2.5),breaks=c(0,0.5,1,1.5,2))+
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(tfp_out_2017_2021_chart, "1.2.1", "total factor productivity 2017 2021")
save_csv(tfp_out_2017_2021, "1.2.1", "total factor productivity 2017 2021")

sources_of_output_growth_by_region_data <- aws.s3::s3read_using(FUN = read_csv,
                                                                bucket = ukfsr::s3_bucket(),
                                                                object = "theme_1/t1_2_3/input/csv/Sources of Output Growth by Region_data.csv")

sources_of_output_growth_by_region_data_world<-sources_of_output_growth_by_region_data%>%
  filter(Region=="World")%>%
  rename(decade=Decade)%>%
  rename(`avg. growth (%/year)`=`Avg. Growth (%/year)`)%>%
  rename(`sources of output growth`=`Sources of Output Growth`)

sources_of_output_growth_by_region_chart<-ggplot()+
  geom_col(data=sources_of_output_growth_by_region_data_world,aes(x = decade, y = `avg. growth (%/year)`*100,fill=`sources of output growth`)) +
  scale_y_continuous(breaks=seq(0,3,0.5))+
  scale_fill_manual(values = af_colours("categorical"),n=4) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(sources_of_output_growth_by_region_chart, "1.2.1", "total factor productivity world")
save_csv(sources_of_output_growth_by_region_data_world, "1.2.1", "total factor productivity world")
