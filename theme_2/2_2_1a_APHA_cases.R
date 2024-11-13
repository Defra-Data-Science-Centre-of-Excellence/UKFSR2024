library(aws.s3)
library(aws.ec2metadata)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(ukfsr)
library(afcolours)

#setwd("~/UKFSR/theme 2/files")

source(here::here("utils", "load-font.R"))

colspec <- cols(
  year = col_integer(),
  status = col_character(),
  value = col_double()
)

#cases <- read_csv("2_2_1a_APHA_cases.csv", col_types = colspec)

cases <- aws.s3::s3read_using(FUN = read_csv,
                              bucket = ukfsr::s3_bucket(),
                              object = "theme_2/input_data/t2_2_1/fsi_8_1_apha_cases.csv", 
                              col_types = colspec)


APHA_cases_chart <- cases |>
  filter(year<2024) |> 
  mutate(status = factor(status, levels = c("not_confirmed", "confirmed"), labels = c("Not confirmed", "Confirmed"), ordered = TRUE), year=as.factor(year)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = status)) +
  scale_x_discrete(breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  scale_fill_manual(values = af_colours(type = "duo")) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr()

APHA_cases_chart

save_graphic(APHA_cases_chart, "2.2.1a", "notifiable animal disease report cases")
save_csv(cases, "2.2.1a", "notifiable animal disease report cases")

#ggsave(filename = "2_2_1a_APHA_cases.svg",
#       APHA_cases_chart,
#       width = 960,
#       height = 640,
#       units = "px",
#       dpi = 72)

#for(i in c(14, 16,22)) {
  
  #cht <- chart + theme_ukfsr(base_family = "GDS Transport Website",
                             #base_size = i,
                             #chart_line_size = 2) +
    #theme(plot.margin = margin(5,50,5,5,unit = "pt"))
  
  #save_graphic(cht, "fsi.8.1", paste("biosecurity fsi base", i))
#}