library(here)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(janitor)
library(scales)
library(purrr)
library(lubridate)
library(countrycode)
library(mm23)
library(ukfsr)
library(data.table)
library(zoo)
library(afcolours)

source(here("utils", "load-font.R"))

t3_1_3c <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_3/output/csv/3_1_3c_local_authority_resourcing.csv")

t3_1_3c$`Financial Year` <- factor(t3_1_3c$`Financial Year`, levels = c("2010/11","2011/12","2012/13","2013/14","2014/15","2015/16","2016/17","2017/18",
                                                                    "2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"))

t3_1_3c_long <- t3_1_3c |> 
  group_by(`Financial Year`) |>
  pivot_longer(cols=c("Allocated Food Hygiene FTE","Allocated Food Standards FTE"),
               names_to="Allocated FTE",
               values_to="Value")

t3_1_3c_long$`Allocated FTE` <- factor(t3_1_3c_long$`Allocated FTE`, levels = c("Allocated Food Hygiene FTE","Allocated Food Standards FTE"))

level_order <- c("Allocated Food Hygiene FTE","Allocated Food Standards FTE")

af_categorical_colours_2 <- afcolours::af_colours("duo")
names(af_categorical_colours_2)=levels(t3_1_3c_long$`Allocated FTE`)


# wrapping of a single word
# https://stackoverflow.com/questions/51515890/stringrstr-wrap-does-not-wrap-exactly-every-n-characters
t3_1_3c_long$Year_wrap = str_replace_all(t3_1_3c_long$`Financial Year`, paste0("(.{5})"), "\\1\n")

t3_1_3c_plot <- ggplot(t3_1_3c_long, aes(x=`Year_wrap`, y=Value, group=`Allocated FTE`, colour=`Allocated FTE`)) +
  geom_line(linewidth=1) +
  geom_text(aes(label = comma(Value)), size = 7, vjust = -1, show.legend  = F) +
  scale_colour_manual(values = af_categorical_colours_2) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(limits = c(0,2000),breaks = seq(0,2000, 200), labels = scales::comma_format()) +
  labs(y = "Allocated FTE") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=20, face = "italic")) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

t3_1_3c_plot

save_graphic(t3_1_3c_plot, "3.1.3c", "local authority resourcing")

save_csv(t3_1_3c, "3.1.3c", "local authority resourcing")

