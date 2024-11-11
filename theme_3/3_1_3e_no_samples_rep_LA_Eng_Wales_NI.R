library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(patchwork)
library(grid)
library(gridtext)
library(gtable)
library(ggtext)
library(data.table)
library(ggalluvial)
library(scales)
library(ggrepel)

source(here("utils", "load-font.R"))

t3_1_3e <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/t3_1_3/output/csv/3_1_3e_no_samples_rep_LA_Eng_Wales_NI.csv")

t3_1_3e$`Financial Year` <- factor(t3_1_3e$`Financial Year`, levels = c("2013/14","2014/15","2015/16","2016/17","2017/18","2018/19","2019/20","2020/21","2021/22","2022/23","2023/24"))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

# wrapping of a single word
# https://stackoverflow.com/questions/51515890/stringrstr-wrap-does-not-wrap-exactly-every-n-characters
t3_1_3e$Year_wrap = str_replace_all(t3_1_3e$`Financial Year`, paste0("(.{5})"), "\\1\n")

t3_1_3e_plot <- ggplot(t3_1_3e, aes(x=`Year_wrap`, y= `Number of samples reported`, group =1)) +
  geom_line(linewidth=1) +
  geom_text(aes(label = comma(`Number of samples reported`)), size = 7, vjust = -1, show.legend  = F) +  
  scale_colour_manual(values = af_colours_1) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000,10000), labels = comma) +
  labs(y = "Number of samples reported") +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=20, face = "italic")) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_line(color = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

t3_1_3e_plot

save_graphic(t3_1_3e_plot, "3.1.3e", "no samples rep LA Eng Wales NI")

save_csv(t3_1_3e, "3.1.3e", "no samples rep LA Eng Wales NI")

