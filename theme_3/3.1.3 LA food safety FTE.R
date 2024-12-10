library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)

source(here::here("utils", "load-font.R"))


# Allocated hygiene and standards E, W, NI -------------------------------------
la_fte <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/la_allocated_food_safety_fte.csv")

cht <-
la_fte |> 
  ggplot() +
  geom_line(aes(x = fye, y = value, group = category, colour = category)) +
  geom_text(aes(x = fye, y = value, group = category, colour = category, label = scales::comma(value)), 
            vjust = -1,
            size = 6,
            family = "GDS Transport Website",show.legend = FALSE) +
  scale_x_discrete(labels = c("2010/11", "", "2012/13", "", "2014/15", "", "2016/17", "", "2018/19", "", "2020/21", "", "2022/23", "")) +
  scale_y_continuous(limits = c(0,2000), labels = scales::label_comma(), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(values = af_colours("duo")) +
  labs(x = NULL, y = "FTE posts") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(cht, "3.1.3c", "la allocated food safety fte e w ni")
save_csv(la_fte, "3.1.3c", "la allocated food safety fte e w ni")


# Allocated hygiene and standards Scotland -------------------------------------

la_fte_scot <- aws.s3::s3read_using(FUN = readr::read_csv,
                               bucket = "s3-ranch-054",
                               object = "theme_3/input_data/3_1_3d_no_alloc_food_law_officers_fte_scot_2016_2024.csv")

cht_scot <- 
la_fte_scot |> 
  ggplot() +
  geom_col(aes(x = Year, y = `Food Law Officers FTE`), fill = af_colours()[1]) +
  geom_text(aes(x = Year, y = `Food Law Officers FTE`,label=`Food Law Officers FTE`), vjust= 1.5, hjust = 0.5, size = 7, colour = "white", family = "GDS Transport Website") +
  scale_x_continuous(breaks = seq(2016,2024, by =1)) +
  scale_y_continuous(breaks = seq(0,300, by = 50), limits = c(0, 300),  expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE)

save_graphic(cht_scot, "3.1.3d", "la allocated food safety fte scot")
save_csv(la_fte_scot, "3.1.3d", "la allocated food safety fte scot")

# Unfilled hygiene posts -------------------------------------------------------

unf_fs <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/input_data/3_1_3d_local_authority_unfilled_food_standards_posts.csv")
unf_fs_plot <- 
unf_fs |> 
  ggplot() +
  geom_line(aes(x=`Financial Year`, y=`Unfilled FS FTE`, group = 1), colour = af_colours()[1]) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5), expand = expansion(mult = c(0, 0.05))) +
  labs(y = "Unfilled Food Standards posts (FTE)", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(unf_fs_plot, "3.1.3e", "local authority unfilled food standards posts")
save_csv(unf_fs, "3.1.3e", "local authority unfilled food standards posts")


# Unfilled standards posts -------------------------------------------------------

unf_fh <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/input_data/3_1_3e_local_authority_unfilled_food_hygiene_posts.csv")

unf_fh_plot <- 
unf_fh |> 
  ggplot() +
  geom_line(aes(x=`Financial Year`, y=`Unfilled FH FTE`, group = 1), colour = af_colours()[1]) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7), expand = expansion(mult = c(0, 0.05))) +
  labs(y = "Unfilled Food Hygiene posts (FTE)", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(unf_fh_plot, "3.1.3f", "local authority unfilled food hygiene posts")
save_csv(unf_fh, "3.1.3f", "local authority unfilled food hygiene posts")

# samples reported E, W, NI ----------------------------------------------------

samples <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = "theme_3/input_data/3_1_3e_no_samples_rep_la_eng_wales_ni.csv")
samples_cht <- 
samples |> 
  ggplot() +
  geom_line(aes(x = `Financial Year`, y = `Number of samples reported`, group = 1), colour = af_colours()[1]) +
  geom_text(aes(x = `Financial Year`, y = `Number of samples reported`, label = scales::comma(`Number of samples reported`)),
            vjust = -1,
            size = 7,
            family = "GDS Transport Website") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 70000), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c("2013/14","2015/16", "2017/18", "2019/20", "2021/22", "2023/24")) +
  labs(y = "Number of samples reported", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(samples_cht, "3.1.3e", "samples reported by las e w ni")
save_csv(samples, "3.1.3e", "samples reported by las e w ni")


# samples reported Scotland ----------------------------------------------------

samples_scot <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_3/input_data/3_1_3f_no_samples_rep_la_scot.csv")
samples_scot_cht <- 
samples_scot |> 
  ggplot() +
  geom_line(aes(x = `Financial Year`, y = `Number of samples reported`, group = 1), colour = af_colours()[1]) +
  geom_text(aes(x = `Financial Year`, y = `Number of samples reported`, label = scales::comma(`Number of samples reported`)),
            vjust = -1,
            size = 7,
            family = "GDS Transport Website") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(breaks = c("2013/14","2015/16", "2017/18", "2019/20", "2021/22", "2023/24")) +
  labs(y = "Number of samples reported", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(samples_scot_cht, "3.1.3f", "samples reported by las scot")
save_csv(samples_scot, "3.1.3f", "samples reported by las scot")
