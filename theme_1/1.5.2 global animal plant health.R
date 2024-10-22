### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

#FAOSTAT

source(here("utils", "load-font.R"))

bananas <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_1/input_data/t1_5_2/banana_production.csv")%>%
  mutate(Area=as.factor(Area))%>%
  mutate(Item=as.factor(Item))

bananas_chart <- bananas|>
  ggplot() +
  geom_line(aes(x = Year, y = Value/1E6, colour = Item), lwd = 1) +
  scale_x_continuous(limits = c(1961,2022),breaks =seq(1965,2022,5)) +
  scale_colour_manual(values = af_colours("duo"))+#,limits=c("South America","Africa","Asia","Northern America","Australia and New Zealand","Europe")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "Million Tonnes")

save_graphic(bananas_chart, "1.5.2c", "global banana production")
save_csv(bananas, "1.5.2c", "global banana production")
