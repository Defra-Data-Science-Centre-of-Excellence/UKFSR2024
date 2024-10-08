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
                                    object = "theme_1/t1_1_1/input/csv/kcal_capita_day.csv")%>%
  filter(Year<2010)

kcalppns <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_1_1/input/csv/kcal_capita_day_nd.csv")

kcalpp<-rbind(kcalpp_temp,kcalppns)%>%
  select(Year,Value,Element)%>%
  rename(year=Year)%>%
  rename(value=Value)%>%
  rename(element=Element)%>%
  filter(year>2010)

kcalpp_chart <- kcalpp |> 
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
save_csv(kcalpp, "1.1.1", "global food supply")

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
  scale_x_continuous(breaks=seq(2011,2021,2)) +
  scale_y_continuous(limits = c(2500,3000), labels = scales::label_comma()) +
  scale_colour_manual(values = af_colours("duo")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "kcals/person/day")

for(i in c(14, 16,22)) {
  
  cht <- fsi1 + theme_ukfsr(base_family = "GDS Transport Website",
                            base_size = i,
                            chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.1.1", paste("global food supply fsi base", i))
  
}

# -----------------------------------------------------------------

cereal_production <- aws.s3::s3read_using(FUN = read_csv,
                                          bucket = ukfsr::s3_bucket(),
                                          object = "theme_1/t1_1_1/input/csv/cereals_production.csv")



fsi1a <- cereal_production |>
  mutate(Item=if_else(Item=="Maize (corn)","Maize",Item)) |>
  mutate(Item=if_else(Item=="Cereals, primary","All Cereals",Item)) |>
  mutate(Item=as.factor(Item)) |>
  ggplot() +
  geom_line(aes(x = Year, y = Value2, colour = Item)) +
  scale_x_continuous(limits = c(2013,2022),breaks =seq(2013,2022,2)) +
  scale_colour_manual(values = af_colours("categorical"),limits=c("All Cereals","Maize","Rice","Wheat","Soya beans","Barley")) +
  labs(x = NULL,
       y = "kg per person per year") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.key.width = unit(58, "pt"))

for(i in c(14, 16, 22)) {
  
  cht <- fsi1a + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt")) +
    theme(legend.key.width = unit(i*2, "pt"))
  
  save_graphic(cht, "fsi.1.1a", paste("cereal production per capita fsi base", i))
  
}



# ------------------------------------------------------------------------------