library(dplyr)
library(tidyr)
library(ggplot2)
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
  #filter(year>2010)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  geom_vline(xintercept =2014,linetype="dashed")+
  geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_y_continuous(limits = c(2000,3000)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/capita/day")


save_graphic(kcalpp_chart, "1.1.1", "global food supply")

production_index <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_1/output/csv/Gross_per_capita_Production_Index.csv")

production_index_chart <- production_index |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |>
  mutate(element="Gross per capita production") |>
  select(year,value,element)  |>
  #filter(year>2011)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element), lwd = 1) +
  scale_y_continuous(limits = c(60,110)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "production index number (2014-2016=100)")

save_graphic(production_index_chart, "1.1.1", "global food production")





# FSI Indicator 1 --------------------------------------------------------------

source(here::here("utils", "load-font.R"))

fsi1 <- kcalpp |> 
  rename(year=Year) |>
  rename(value=Value) |> 
  rename(element=Element) |> 
  select(year,value,element)  |>
  filter(year>2010)|>
  ggplot() +
  geom_line(aes(x = year, y = value, colour = element)) +
  # geom_vline(xintercept =2014,linetype="dashed")+
  # geom_text(aes(x=2009,y=2500,label="change in\ncals/capita/day\nmethodology"),size=4)+
  scale_y_continuous(limits = c(2500,3000)) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/capita/day")

for(i in c(16,22)) {
  
  cht <- fsi1 + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt"))
  
  save_graphic(cht, "fsi.1.1", paste("global food supply fsi base", i))
  
}
