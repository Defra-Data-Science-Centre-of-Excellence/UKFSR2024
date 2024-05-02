library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here("utils", "load-font.R"))

kcalpp_temp <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/kcal_capita_day.csv")%>%
                            filter(Year<2010)

kcalppns <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/kcal_capita_day_nd.csv")

kcalpp<-rbind(kcalpp_temp,kcalppns)

kcalpp_chart <- kcalpp |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |> 
  select(year,value,element)  |>
  filter(year>2010)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  #geom_vline(xintercept =2014,linetype="dashed")+
  #geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_y_continuous(limits = c(2850,3000)) +
  scale_x_continuous(limits = c(2012,2021),breaks =seq(2012,2021,1)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/person/day")


save_graphic(kcalpp_chart, "1.1.1", "global food supply")

# FSI Indicator 1 --------------------------------------------------------------

source(here::here("utils", "load-font.R"))

fsi1 <- kcalpp |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |> 
  select(year,value,element)  |>
  filter(year>2010)|>
  mutate(element = factor(element, levels= c("Food supply (kcal/capita/day)"),
                          labels= c("Food supply (kcal/person/day)"))) |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element)) +
  # geom_vline(xintercept =2014,linetype="dashed")+
  # geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_y_continuous(limits = c(2500,3000), labels = scales::label_comma()) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/person/day")

for(i in c(16,22)) {
  
  cht <- fsi1 + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt"))
  
  save_graphic(cht, "fsi.1.1", paste("global food supply fsi base", i))
  
}

# -----------------------------------------------------------------

production_index <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/Gross_per_capita_Production_Index.csv")

production_index_chart <- production_index |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  mutate(element="Gross per capita production") |>
  select(year,value,element)  |>
  filter(year>2011)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  scale_y_continuous(limits = c(60,110)) +
  scale_x_continuous(limits = c(2011.5,2022.5),breaks =seq(2012,2022,1)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production index number\n(2014-2016=100)")

save_graphic(production_index_chart, "1.1.1", "global food production")



cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/t1_1_1/output/csv/cereals_production.csv")

cereal_production_chart <- cereal_production |>
  mutate(Item=if_else(Item=="Maize (corn)","Maize",Item)) |>
  mutate(Item=if_else(Item=="Cereals, primary","Cereals",Item)) |>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1e6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,1)) +
  scale_colour_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(cereal_production_chart, "1.1.1", "global cereal production")

use_of_agricultural_commodities_by_type_and_region <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_1_1/input/csv/use_of_agricultural_commodities_by_type_and_region.csv")

use_of_agricultural_commodities_by_type_and_region_chart <- use_of_agricultural_commodities_by_type_and_region |>
  pivot_longer(cols=c('food','feed','biofuel','other'),names_to = "item",values_to = "value") |>
  ggplot(aes(fill=item, y=value, x=commodity)) +
  geom_bar(position = "fill", stat="identity")+
  coord_flip()+
  scale_fill_manual(values = af_colours("categorical")) +
  scale_y_continuous(labels=c("0","25%","50%","75%","100%"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(use_of_agricultural_commodities_by_type_and_region_chart, "1.1.1", "use of agricultural commodities by type and region")



feed_demand_by_component_and_by_region <- aws.s3::s3read_using(FUN = read_csv,
                                                                           bucket = ukfsr::s3_bucket(),
                                                                           object = "theme_1/t1_1_1/input/csv/Feed demand by component and by region.csv")

feed_demand_by_component_and_by_region_chart <- feed_demand_by_component_and_by_region |>
  ggplot(aes(fill=`feed type`, y=value, x=area)) +
  geom_bar(position = "stack", stat="identity")+
  coord_flip()+
  scale_fill_manual(values = af_colours("categorical")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(feed_demand_by_component_and_by_region_chart, "1.1.1", "feed demand by component and by region")

global_cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                                               bucket = ukfsr::s3_bucket(),
                                                               object = "theme_1/t1_1_1/input/csv/cerealsproduction.csv")

global_cereal_production_chart <- ggplot(data=global_cereal_production) +
  geom_line(aes(x=year ,y=value/1E3,color=commodity,linetype=commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_x_continuous(breaks = c(2015,2017,2019,2021,2023),labels=c("2015/2016","2017/2018","2019/2020","2021/2022","2023/2024"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "1,000,000 Million tonnes")

save_graphic(global_cereal_production_chart, "1.1.1", "global cereal production")


global_biofuel_production <- aws.s3::s3read_using(FUN = read_csv,
                                                 bucket = ukfsr::s3_bucket(),
                                                 object = "theme_1/t1_1_1/input/csv/biofuelpercentageproduction.csv")

global_biofuel_production_chart <- ggplot(data=global_biofuel_production) +
  geom_line(aes(x=year ,y=percentage,color=commodity))+
  scale_color_manual(values = af_colours("categorical")) +
  scale_y_continuous(limits=c(0,30))+
  scale_x_continuous(breaks = seq(2010,2023,1))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Biofuel demand share of\nglobal crop production")

save_graphic(global_biofuel_production_chart, "1.1.1", "global biofuel production")

