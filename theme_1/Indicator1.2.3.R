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

#tfp <- aws.s3::s3read_using(FUN = read_excel,
#                                        bucket = ukfsr::s3_bucket(),
#                                        object = "theme_1/t1_2_3/input/csv/ERSTFPAgriculture.xlsx")

#tfp_out<-tfp%>%
#  pivot_longer(cols=3:63,values_to = "tfp",names_to="year")%>%
#  mutate(year=as.numeric(year))%>%
#  filter(year>2000)

########

#tfp_chart<-ggplot()+
#  geom_line(data=tfp_out,aes(x = year, y = tfp, colour = `Income Class`)) +
#  #scale_y_continuous(limits = c(0,50)) +
#  scale_x_continuous(limits = c(2001,2021),breaks=seq(2001,2021,2))+
#  #scale_colour_manual(values = af_colours("categorical",n=6),limits=c("Oilseed, Soybean","Wheat","Barley","Maize","Rice, Milled")) +
#  theme_ukfsr(base_family = "GDS Transport Website") +
#  labs(x = NULL,
#       y = "")

#save_graphic(tfp_chart, "1.2.3", "total factor productivity")
#save_csv(tfp_out, "1.2.3", "total factor productivity")

tfp_out_2017_2021 <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_2_3/input/csv/agricultural_productivity.csv")


tfp_out_2017_2021_chart<-ggplot()+
  geom_col(data=tfp_out_2017_2021,aes(x = year, y = growth,fill=year)) +
  facet_wrap(~`income group`)+
  #scale_y_continuous(limits=c(95,115))+
  scale_fill_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(tfp_out_2017_2021_chart, "1.2.3", "total factor productivity 2017 2021")
save_csv(tfp_out_2017_2021, "1.2.3", "total factor productivity 2017 2021")

sources_of_output_growth_by_region_data <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_2_3/input/csv/Sources of Output Growth by Region_data.csv")

sources_of_output_growth_by_region_data_world<-sources_of_output_growth_by_region_data%>%
  filter(Region=="World")

sources_of_output_growth_by_region_chart<-ggplot()+
  geom_col(data=sources_of_output_growth_by_region_data_world,aes(x = Decade, y = `Avg. Growth (%/year)`*100,fill=`Sources of Output Growth`)) +
  scale_y_continuous(breaks=seq(0,3,0.5))+
  scale_color_manual(values = af_colours("categorical"),n=4) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Average Annual Growth (%)")

save_graphic(sources_of_output_growth_by_region_chart, "1.2.3", "total factor productivity world")
save_csv(sources_of_output_growth_by_region_data_world, "1.2.3", "total factor productivity world")

employment_data <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_3/input/csv/EmploymentIndicators.csv")%>%
  filter(Year>2000)

employment_data_chart<-ggplot()+
  geom_line(data=employment_data,aes(x = Year, y = Value/1E3,color=`Area`)) +
  scale_color_manual(values = af_colours("categorical"),n=6) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Number of Employees (millions)")


save_graphic(employment_data_chart, "1.2.3", "agricultural employment")
save_csv(employment_data, "1.2.3", "agricultural employment")

agricultural_value_added <- aws.s3::s3read_using(FUN = read_csv,
                                   bucket = ukfsr::s3_bucket(),
                                   object = "theme_1/t1_2_3/input/csv/AgricultureValueAdded.csv")%>%
  select(Area,Year,Value)

gdp <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_2_3/input/csv/GDP.csv")%>%
  select(Area,Year,Value)

population <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/t1_2_3/input/csv/Population.csv")%>%
  select(Area,Year,Value)

africa <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/africa_list.csv")

asia <- aws.s3::s3read_using(FUN = read_csv,
                             bucket = ukfsr::s3_bucket(),
                             object = "theme_1/t1_1_1/input/csv/asia_list.csv")

europe <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_1/t1_1_1/input/csv/europe_list.csv")

latin_america_caribbean <- aws.s3::s3read_using(FUN = read_csv,
                                                bucket = ukfsr::s3_bucket(),
                                                object = "theme_1/t1_1_1/input/csv/latin_america_list.csv")

north_america <- aws.s3::s3read_using(FUN = read_csv,
                                      bucket = ukfsr::s3_bucket(),
                                      object = "theme_1/t1_1_1/input/csv/north_america_list.csv")

oceania <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_1/t1_1_1/input/csv/oceania_list.csv")

world_key<-rbind(north_america,oceania,latin_america_caribbean,europe,asia,africa)%>%
  select(Area,Region)


value_added<-agricultural_value_added%>%
  left_join(gdp,by=c("Year"="Year","Area"="Area"))%>%
  left_join(population,by=c("Year"="Year","Area"="Area"))%>%
  left_join(world_key,by=c("Area"="Area"))%>%
  rename(value_added=`Value.x`)%>%
  rename(gdp=`Value.y`)%>%
  rename(population=`Value`)

value_added_chart<-ggplot(value_added)+
  geom_point(aes(x=log10(gdp),y=log10(value_added),size=population/1E3,color=Region))+
  scale_x_continuous(limits=c(2.6,5.2),breaks = c(2.69,3,3.3,3.7,4,4.3,4.7,5),labels=c("$500","$1000","$2000","$5000","$10,000","$20,000","$50,000","$100,000"))+
  scale_y_continuous(limits=c(2,5.2),breaks=c(2,3,4,5),labels=c("100","1000","10,000","100,000"))+
  scale_color_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = "GDP per capita (USD)",
       y = "value added per worker \n US$ constant 2015")

save_graphic(value_added_chart, "1.2.3", "value added chart")
save_csv(value_added, "1.2.3", "value added")
