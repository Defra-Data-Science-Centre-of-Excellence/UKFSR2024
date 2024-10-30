#devtools::install_github("FoodchainStats/ukfsr")


library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)

source(here::here("utils", "load-font.R"))

net_trade <- function(file, product) {

  data <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = file)
  
  data <- data |> 
    clean_names() |> 
    separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
    group_by(year, flow) |> 
    summarise(value = sum(net_mass_kg)/1000000) |> 
    pivot_wider(names_from = flow) |> 
    rename(imports = Imports,
           exports = Exports) |> 
    mutate(net = imports - exports,
           product = product) |> 
    pivot_longer(cols = c(exports, imports, net),names_to = "flow")
  
  return(data)
  
}


country_trade <- function(file, product) {
  
  data <- aws.s3::s3read_using(FUN = read_csv,
                               bucket = ukfsr::s3_bucket(),
                               object = file)
  
  data <- data |> 
    clean_names() |> 
    separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
    group_by(year, country, flow) |> 
    summarise(value = sum(net_mass_kg)/1000000) |> 
    mutate(product = product)
  
  return(data)
}



# Final graphics----------------------------------------------------------------

# Wheat net trade---------------------------------------------------------------
wheat_net <- net_trade("theme_3/input_data/3_1_11f_wheat_trade_data.csv", "wheat")

wheat_net_cht <- wheat_net |> 
  filter(flow == "net") |> 
  ggplot() +
  geom_col(aes(x = year, y = value), fill = af_colours(n =1)) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(wheat_net_cht, "3.1.2b", "wheat net trade final")
save_csv(wheat_net, "3.1.2b", "wheat net trade final")

# Combined net data ------------------------------------------------------------

co2_net <- net_trade("theme_3/input_data/3_1_11a_co2_trade_data.csv", "co2")
hypo_net <- net_trade("theme_3/input_data/3_1_11b_hypochlorite_trade_data.csv", "hypochlorite")
pet_net <- net_trade("theme_3/input_data/3_1_11c_pet_trade_data.csv", "pet")
cardboard_net <- net_trade( "theme_3/input_data/3_1_11d_cardboard_trade_data.csv", "cardboard")
sunflower_net <- net_trade("theme_3/input_data/3_1_11e_sunflower_oil_trade_data.csv", "sunflower")

all_net <- bind_rows(co2_net,hypo_net,pet_net,cardboard_net, sunflower_net) |> 
  filter(flow == "net")


all_net_cht <- all_net |> 
  mutate(product = factor(product,
                          levels = c("co2", "hypochlorite", "cardboard", "pet", "sunflower"),
                          labels = c("CO2", "Hypochlorite", "Cardboard", "PET", "Sunflower oil"))) |> 
  ggplot() +
  geom_col(aes(x = year, y = value), fill = af_colours(n =1)) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  facet_wrap(vars(product), ncol = 2) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(all_net_cht, "3.1.2a", "net trade by product final")
save_csv(all_net, "3.1.2a", "net trade by product final")

# OLD WORKINGS -----------------------------------------------------------------
# Trade by country--------------------------------------------------------------

co2_cnt <- country_trade("theme_3/input_data/3_1_11a_co2_trade_data.csv", "co2")
hypo_cnt <- country_trade("theme_3/input_data/3_1_11b_hypochlorite_trade_data.csv", "hypochlorite")
pet_cnt <- country_trade("theme_3/input_data/3_1_11c_pet_trade_data.csv", "pet")
cardboard_cnt <- country_trade( "theme_3/input_data/3_1_11d_cardboard_trade_data.csv", "cardboard")
sunflower_cnt <- country_trade("theme_3/input_data/3_1_11e_sunflower_oil_trade_data.csv", "sunflower")
wheat_cnt <- country_trade("theme_3/input_data/3_1_11f_wheat_trade_data.csv", "wheat")


country_chart <- function(data, threshold = 1) {
  cht <- data |> 
    filter(year > 2020, flow == "Imports") |> 
    group_by(country) |> 
    summarise(value = mean(value)) |> 
    mutate(country = case_when(value <= threshold ~ "Other", .default = country)) |> 
    ggplot() +
    geom_col(aes(x = fct_reorder(country, value), y = value), fill = af_colours(n=1)) +
    labs(y = "kilotonnes", x = NULL) +
    coord_flip() +
    theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE)
  
  return(cht)
}

co2_country_cht <- country_chart(co2_cnt)
save_graphic(co2_country_cht, "3.1.2", "co2 trade by country")

hypo_country_cht <- country_chart(hypo_cnt, threshold = 0.5)
save_graphic(hypo_country_cht, "3.1.2", "hypo trade by country")

pet_country_cht <- country_chart(pet_cnt, threshold = 5)
save_graphic(pet_country_cht, "3.1.2", "pet trade by country")

cardboard_country_cht <- country_chart(cardboard_cnt,threshold = 3)
save_graphic(cardboard_country_cht, "3.1.2", "cardboard trade by country")

sunflower_country_cht <- country_chart(sunflower_cnt)
save_graphic(sunflower_country_cht, "3.1.2", "sunflower trade by country")

wheat_country_cht <- country_chart(wheat_cnt, threshold = 15)
save_graphic(wheat_country_cht, "3.1.2", "wheat trade by country")


x <- co2_cnt  |> 
  filter(year > 2020, flow == "Imports") |> 
  group_by(country) |> 
  summarise(value = mean(value)) |> 
  ungroup() |> 
  mutate(pc = value/sum(value) *100)



# CO2 --------------------------------------------------------------------------

# Original data table from uktradeinfo
# 
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=d39d4559-a1a5-4204-a201-b90496ef63ca

co2 <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/3_1_11a_co2_trade_data.csv")


co2_out <- co2 |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

co2_cht <- co2_out |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(co2_out, "3.1.2a", "CO2 trade")
save_graphic(co2_cht, "3.1.2a", "CO2 trade")

co2_net <- co2_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

co2_net_cht <-   ggplot(co2_net) +
  geom_col(aes(x = year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(co2_net, "3.1.2a", "CO2 net trade")
save_graphic(co2_net_cht, "3.1.2a", "CO2 net trade")


co2_countries <- co2 |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, country, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

co2_country_cht <- co2_countries |> 
  filter(flow == "Imports", year >= 2023) |> 
  mutate(country = case_when(value <= 1 ~ "Other", .default = country)) |> 
  group_by(year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = fct_reorder(country, value) ), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")

save_csv(co2_countries, "3.1.2a", "CO2 trade by country")
save_graphic(co2_country_cht, "3.1.2a", "CO2 imports by country")

# Hypochlorite -----------------------------------------------------------------

# Original data table from uktradeinfo
# 
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=1b7a1a0f-2802-46c9-a1c1-93d8d913d4e6


hypo <- aws.s3::s3read_using(FUN = read_csv,
                             bucket = ukfsr::s3_bucket(),
                             object = "theme_3/input_data/3_1_11b_hypochlorite_trade_data.csv")

hypo_out <- hypo |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000)


hypo_cht <- hypo_out |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(hypo_out, "3.1.2b", "hypochlorite trade")
save_graphic(hypo_cht, "3.1.2b", "hypochlorite trade")


hypo_net <- hypo_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

hypo_net_cht <-   ggplot(hypo_net) +
  geom_col(aes(x = year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(hypo_net, "3.1.2b", "hypochlorite net trade")
save_graphic(hypo_net_cht, "3.1.2b", "hypochlorite net trade")

hypo_countries <- hypo |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, country, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

hypo_country_cht <- hypo_countries |> 
  filter(flow == "Imports", year >= 2023) |> 
  mutate(country = case_when(value <=0.4 ~ "Other", .default = country)) |> 
  group_by(year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = fct_reorder(country, value)), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")

save_csv(hypo_countries, "3.1.2b", "hypochlorite trade by country")
save_graphic(hypo_country_cht, "3.1.2b", "hypochlorite imports by country")

# PET --------------------------------------------------------------------------
# https://www.trade-tariff.service.gov.uk/commodities/3915902000
# https://www.trade-tariff.service.gov.uk/commodities/3926909790
# 
# uktradeinfo table
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=62cfd6e7-7906-4486-9fe6-af75b925d420

pet <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_3/input_data/3_1_11c_pet_trade_data.csv")

pet_out <- pet |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(date_hierarchy_year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000)

pet_cht <- pet_out |> 
  ggplot() +
  geom_col(aes(x = date_hierarchy_year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")  

save_csv(pet_out, "3.1.2c", "pet trade")
save_graphic(pet_cht, "3.1.2c", "pet trade")


pet_net <- pet_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

pet_net_cht <-   ggplot(pet_net) +
  geom_col(aes(x = date_hierarchy_year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(pet_net, "3.1.2c", "pet net trade")
save_graphic(pet_net_cht, "3.1.2c", "pet net trade")


pet_countries <- pet |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(date_hierarchy_year, country_hierarchy_country, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

pet_country_cht <- pet_countries |> 
  filter(flow == "Imports", date_hierarchy_year >= 2023) |> 
  mutate(country = case_when(value <= 11 ~ "Other", .default = country_hierarchy_country)) |> 
  group_by(date_hierarchy_year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = date_hierarchy_year, y = value, fill = fct_reorder(country, value) ), position = position_stack()) +
  scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right")

save_csv(pet_countries, "3.1.2c", "pet trade by country")
save_graphic(pet_country_cht, "3.1.2c", "pet imports by country")

# Cardboard --------------------------------------------------------------------
# 
# uktradeinfo table:
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=40b5158b-6c83-47fe-9b8d-dc5531ef7138
# 
# *Background info from DBT*
# 
# The UK paper and pulp industry contributed £4.0bn to the economy in 2021, with
# a total of 1,470 businesses creating £12.7bn turnover. In 2023 exports were
# worth around £2.8 billion with around 54,000 jobs across the UK.
#
# Regarding commodity codes, there’s a rather complex response below from the
# Confederation of Paper Industries:
#
# It is difficult to give a simple answer to that question, for a number of
# reasons it's actually more complicated.  We follow about 60 different grades
# within Chapter 48.
#
# Is the enquiry specifically about finished packaging, or the papers that make
# up that packaging (which are fundamental)?
# 
# •	Paper based packaging can be what is called cartonboard (or solid board) for
# sandwich packs, food trays, breakfast cereal, confectionery etc. or it can be
# corrugated for fruit & veg trays and pizza boxes, e-commerce/home delivery.
# 
# •	In both cases, it starts as reels of paper before conversion into final
# packaging form.  It is common for cartonboard to be imported as reels of paper
# and converted in UK; corrugated papers are more commonly made in UK (though
# sometimes imported) and almost always converted in UK.
# 
# •	An intermediary stage (after paper but before finished box) for corrugated
# involves flat ‘sheet’ with a three-layer structure
# 
# The key paper/intermediary grades are as follows: 
# 4804 11 kraft paper; also 4804 42, 4804 49, 4804 51
# 4805 11 semi chem fluting; also 4805 19, 4805 24, 4805 25
# 4810 papers coated with china clay
# 4808 10 corrugated sheet
# 
# All of which need to be followed for a full understanding of the paper
# packaging market
# 
# But the simple answer is, for the finished/converted boxes:
# 4819 10 corrugated boxes
# 4819 20 folding cartons
# 
# Be wary of just following those two grades and assuming that it will lead to
# an understanding of paper packaging for food.  Not least because while about
# 65% of UK made corrugated is for food & drink customers, most of that is
# secondary or transport packaging; the majority (>85%) of folding carton board
# will be for food, much of it direct contact.  But these customs codes do not
# differentiate between food & non-food applications.


cardboard <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_3/input_data/3_1_11d_cardboard_trade_data.csv")

cardboard_out <- cardboard |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(date_hierarchy_year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000)

cardboard_cht <- cardboard_out |> 
  ggplot() +
  geom_col(aes(x = date_hierarchy_year, y = value, fill = flow), position = position_dodge()) +
  scale_fill_manual(values = af_colours("duo")) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")  

save_csv(cardboard_out, "3.1.2d", "cardboard trade")
save_graphic(cardboard_cht, "3.1.2d", "cardboard trade")


cardboard_net <- cardboard_out |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)

cardboard_net_cht <-   ggplot(cardboard_net) +
  geom_col(aes(x = date_hierarchy_year, y = net), fill = af_colours()[1]) +
  labs(y = "kilotonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website")

save_csv(cardboard_net, "3.1.2d", "cardboard net trade")
save_graphic(cardboard_net_cht, "3.1.2d", "cardboard net trade")


cardboard_countries <- cardboard |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(date_hierarchy_year, country_hierarchy_country, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) 

cardboard_country_cht <- cardboard_countries |> 
  filter(flow == "Imports", date_hierarchy_year >= 2023) |> 
  mutate(country = case_when(value <= 1 ~ "Other", .default = country_hierarchy_country)) |> 
  group_by(date_hierarchy_year, country) |> 
  summarise(value = sum(value)) |> 
  ggplot() +
  geom_col(aes(x = fct_reorder(country, value), y = value ), fill = af_colours(n =1), position = position_dodge()) +
  # scale_x_continuous(breaks = c(2023)) +
  labs(y = "kilotonnes", x = NULL) +
  scale_fill_manual(values = rev(af_colours())) +
  theme_ukfsr(base_family = "GDS Transport Website") + theme(legend.position = "right") +
  coord_flip()

save_csv(cardboard_countries, "3.1.2d", "cardboard trade by country")
save_graphic(cardboard_country_cht, "3.1.2d", "cardboard imports by country")


# Wheat ------------------------------------------------------------------------
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=dd25c5cd-0669-47c2-bd73-246b8cb4bc13

wheat <- aws.s3::s3read_using(FUN = read_csv,
                              bucket = ukfsr::s3_bucket(),
                              object = "theme_3/input_data/3_1_11f_wheat_trade_data.csv")

wheat |> 
  clean_names() |> 
  separate_wider_delim(flow_type, delim = " - ", names = c("area", "flow")) |> 
  group_by(year, flow) |> 
  summarise(value = sum(net_mass_kg)/1000000) |> 
  pivot_wider(names_from = flow) |> 
  mutate(net = Imports - Exports)


# Sunflower oil ----------------------------------------------------------------
# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=e7b2d3c5-42b8-41d8-8dd2-defbec251369


sunflower <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_3/input_data/3_1_11e_sunflower_oil_trade_data.csv")








# --------------------------------------------------------------------------------

# Wheat UK Import/Export data.


FSR_3_1_2f <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/UK Wheat trade data.csv")

FSR_3_1_2f <- FSR_3_1_2f %>%
  gather(variable,value,`Export`,`Import`)

FSR_3_1_2f_plot <- ggplot(FSR_3_1_2f, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  labs(x = NULL,
       y = "Thousand tonne") +
  scale_colour_manual(values = af_colours()) + 
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  guides(colour=guide_legend(override.aes=list(size=1)))+
  expand_limits(y = 0) 

FSR_3_1_2f_plot

save_csv(FSR_3_1_2f, "3.1.2f", "UK Wheat import and export")
save_graphic(FSR_3_1_2f_plot, "3.1.2f", "UK Wheat import and export")