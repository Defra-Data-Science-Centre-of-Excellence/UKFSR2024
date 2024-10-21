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

tfp_out_2013_2022 <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_2_1/input/csv/agricultural_productivity_2.csv")%>%
  mutate(Region=if_else(Region=="Upper-middle income, excluding China","Upper-middle\nincome,\nexcluding China",if_else(Region=="Upper-middle income","Upper-middle\nincome",if_else(Region=="Lower-middle income","Lower-middle\nincome",Region))))


tfp_out_2013_2022_chart<-ggplot()+
  geom_col(data=tfp_out_2013_2022,aes(x = Year, y = Annual_Percentage,fill=Year)) +
  facet_wrap(~Region)+
  scale_y_continuous(limits=c(0,2.5),breaks=c(0,0.5,1,1.5,2))+
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(tfp_out_2013_2022_chart, "1.2.1b", "total factor productivity 2013 2022")
save_csv(tfp_out_2013_2022, "1.2.1b", "total factor productivity 2013 2022")

sources_of_output_growth_by_region_data <- aws.s3::s3read_using(FUN = read_csv,
                                                                bucket = ukfsr::s3_bucket(),
                                                                object = "theme_1/t1_2_1/input/csv/TFP.csv")

sources_of_output_growth_by_region_data_world<-sources_of_output_growth_by_region_data%>%
  filter(!Element=="Total agricultural output growth rate")

sources_of_output_growth_by_region_chart<-ggplot()+
  geom_col(data=sources_of_output_growth_by_region_data_world,aes(x = Date, y = Percentage,fill=Element)) +
  scale_y_continuous(breaks=seq(0,6,0.5))+
  scale_fill_manual(values = af_colours("categorical"),n=4) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill=guide_legend(nrow=4, byrow=TRUE))+
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(sources_of_output_growth_by_region_chart, "1.2.1a", "total factor productivity world")
save_csv(sources_of_output_growth_by_region_data_world, "1.2.1a", "total factor productivity world")
