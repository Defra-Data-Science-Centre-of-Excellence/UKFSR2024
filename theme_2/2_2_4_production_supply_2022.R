library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(aws.ec2metadata)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(readODS)
library(glue)
library(scales)
library(plotly)
library(forcats)

source(here("utils", "load-font.R"))

## - - - - - - - - - - - - - -
## Functions:

## Round number ----

round_number <- function(val, prefix = NULL, suffix = NULL, big.mark = ""){
  
  case_when(abs(val) < 10 ~ label_number(accuracy = 0.1, 
                                         prefix = prefix, suffix = suffix, big.mark = big.mark)(val),
            abs(val) >= 10 ~ label_number(accuracy = 1, 
                                          prefix = prefix, suffix = suffix, big.mark = big.mark)(val))
}

## vline function ----
vline <- function(x = 0, color = "black") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1.1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = "dash")
  )
}

## - - - - - - - - - - - - - -


# hard coded variables ----------------------------------------------------

# set current year
year <- 2022

# govuk palette
govuk_pal <- c("#12436D", "#28A197", "#801650", "#F46A25", "#3D3D3D", "#A285D1")

# data load ---------------------------------------------------------------

## Chapter 7 data ----

# cereals
cereals_prod <- aws.s3::s3read_using(FUN = read_ods,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                            sheet = "Total_cereals_(Table_7_1)", 
                            col_names = TRUE, 
                            skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...2 == "Value of production (£ million) (b)") %>%
  mutate(item = "Cereals") %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)

# oilseed  
osr_prod <- aws.s3::s3read_using(FUN = read_ods,
                     bucket = ukfsr::s3_bucket(),
                     object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                     sheet = "Oilseed_(Table_7_5)", 
                     col_names = TRUE, 
                     skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...2 == "Value of production (£ million) (b)") %>%
  mutate(item = "Oilseed rape") %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)


# Sugar beet
sugarbeet_prod <- aws.s3::s3read_using(FUN = read_ods,
                           bucket = ukfsr::s3_bucket(),
                           object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                           sheet = "Sugar_beet_(Table_7_7)", 
                           col_names = TRUE, 
                           skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...2 == "Value of production (£ million)") %>%
  mutate(item = "Sugar beet") %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)


# Fresh veg
veg_prod <- aws.s3::s3read_using(FUN = read_ods,
                     bucket = ukfsr::s3_bucket(),
                     object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                     sheet = "Fresh_veg_(Table_7_9)", 
                     col_names = TRUE, 
                     skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...2 == "Value of production (£ million):") %>%
  mutate(item = "Fresh vegetables") %>% 
  select(item, glue("{year}")) %>% 
  filter(row_number() == 1) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)


# Fresh fruit
fruit_prod <- aws.s3::s3read_using(FUN = read_ods,
                       bucket = ukfsr::s3_bucket(),
                       object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                       sheet = "Fresh_fruit_(Table_7_12)", 
                       col_names = TRUE, 
                       skip = 8) %>%
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...1 == "Value of production (£ million) (c) (e):") %>%
  mutate(item = "Fresh fruit") %>% 
  select(item, glue("{year}"))%>% 
  filter(row_number() == 1) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)


# Potatoes
pot_prod <- aws.s3::s3read_using(FUN = read_ods,
                     bucket = ukfsr::s3_bucket(),
                     object = "theme_2/t2_2_4/input/csv/AUK-Chapter7-13jul23.ods",
                     sheet = "Potatoes_(Table_7_11)", 
                     col_names = TRUE, 
                     skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...2 == "Value of production (£ million)") %>%
  mutate(item = "Potatoes") %>% 
  select(item, glue("{year}")) %>% 
  filter(row_number() == 1) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)

## Chapter 8 data ----

# Meat
meat_prod <- aws.s3::s3read_using(FUN = read_ods,
                      bucket = ukfsr::s3_bucket(),
                      object = "theme_2/t2_2_4/input/csv/AUK-Chapter8-13jul23.ods",
                      sheet = "Meat_production_(Table_8_1)", 
                      col_names = TRUE, 
                      skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(row_number() %in% c(8:11)) %>% 
  rename(item = ...1) %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)

# Milk
milk_prod <- aws.s3::s3read_using(FUN = read_ods,
                      bucket = ukfsr::s3_bucket(),
                      object = "theme_2/t2_2_4/input/csv/AUK-Chapter8-13jul23.ods",
                      sheet = "Milk_(Table_8_6)", 
                      col_names = TRUE, 
                      skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...1 ==  "Value of production (£ million)") %>%
  mutate(item = "Milk") %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)


# Eggs
eggs_prod <- aws.s3::s3read_using(FUN = read_ods,
                      bucket = ukfsr::s3_bucket(),
                      object = "theme_2/t2_2_4/input/csv/AUK-Chapter8-13jul23.ods",
                      sheet = "Hen_eggs_(Table_8_7)", 
                      col_names = TRUE, 
                      skip = 8) %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  filter(...1 ==  "Value of production of eggs for human consumption (£ million) (b)") %>%
  mutate(item = "Eggs") %>% 
  select(item, glue("{year}")) %>% 
  rename(prod = 2) %>% 
  mutate(prod = as.numeric(prod)) %>% 
  select(item, prod)

## Chapter 14 data ----

# raw
table_14_2_raw <- aws.s3::s3read_using(FUN = read_ods,
                           bucket = ukfsr::s3_bucket(),
                           object = "theme_2/t2_2_4/input/csv/AUK-Chapter14-13jul23i.ods",
                           sheet = "Table_14_2",
                           col_names = TRUE,
                           skip = 8)

# clean
table_14_2_clean <- table_14_2_raw %>% 
  rename_with(~str_replace_all(.x, "X", "")) %>% #rename_with to remove "X" prefix from year column names
  rename(item = ...1) %>% 
  drop_na(glue("{year}")) %>%
  pivot_longer(cols = c(`2020`:`2022`), names_to = "year")

# data wrangling ----------------------------------------------------------

# bind data
prod_data <- cereals_prod %>%
  bind_rows(osr_prod) %>%
  bind_rows(sugarbeet_prod) %>%
  bind_rows(veg_prod) %>%
  bind_rows(fruit_prod) %>%
  bind_rows(pot_prod) %>%
  bind_rows(meat_prod) %>%
  bind_rows(milk_prod) %>%
  bind_rows(eggs_prod)

# bar data
bar_data <- table_14_2_clean %>%
  rename(years = year) %>% 
  filter(years == year) %>% 
  filter(item %in% c("Cereals", 
                     "Oilseed rape", 
                     "Sugar beet", 
                     "Fresh vegetables",
                     "Potatoes",
                     "Fresh fruit",
                     "Beef and veal",
                     "Pigmeat",
                     "Poultrymeat",
                     "Mutton and lamb",
                     "Milk",
                     "Eggs")) %>% 
  mutate(item = case_when(item == "Beef and veal" ~ "Cattle",
                          item == "Pigmeat" ~ "Pigs",
                          item == "Mutton and lamb" ~ "Sheep",
                          item == "Poultrymeat" ~ "Poultry",
                          TRUE ~ item),
         value = value * 100) %>% 
  left_join(prod_data, by = "item") %>% 
  mutate(label = str_c("<b>", item, "</b>", "<br>",
                       "Ratio: ", round_number(value), "%", "<br>",
                       "Value of sector: ", label_comma(accuracy = 0.1, prefix = "\u00A3", suffix = " billion", scale = 0.001)(prod)))

# get order
# ratio_order <- bar_data %>% 
  # arrange(value) %>% 
  # pull(item)

bar_data_out<-bar_data%>%mutate(surplus_imports=if_else(value>100,"","")) %>%
  arrange(value)

# chart -------------------------------------------------------------------

ratio_barplot <- ggplot()+
  geom_col(data=bar_data_out, aes(fct_reorder(item, value),value,fill=surplus_imports), show.legend=F)+
  coord_flip()+
  geom_hline(yintercept =100,linetype="dashed")+
  geom_text(data=bar_data_out, aes(fct_reorder(item, value),value, label = round (value,0), color=surplus_imports), hjust = 1.5, show.legend=F)+
  scale_y_continuous(limits = c(0,110),breaks = seq(0,100,20)) +
  scale_fill_manual(values = af_colours("duo")) +
  scale_color_manual(values = c("white", "black")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "percent")

save_graphic(ratio_barplot, "2.2.4", "production supply ratio 2022")