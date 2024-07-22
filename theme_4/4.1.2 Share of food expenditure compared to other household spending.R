# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
#library(aws.ec2.metadata)

library(stringr)
library(data.table)
library(ggrepel)
library(scales)
library(zoo)
library(plyr)
library(grid)
library(ggpp)
library(ggrepel)
library(png)
library(viridis)
library(showtext)
library(svglite)



library(here)
library(sf)
library(janitor)


contents <- get_bucket_df("s3-ranch-054")

source(here::here("utils", "load-font.R"))

F4_1_2 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                 bucket = "s3-ranch-054",
                                 object = "theme_4/input_data/4_1_2a_ave_spend_food_non_alcohol_drinks_low_income_all_households_middle_income.csv")


FSR_4_1_2 <- F4_1_2 %>% 
  gather(key,value, `percentage spend on food and non-alcoholic drinks for all households`,`percentage spend on food and non-alcoholic drinks for middle 20% by income`, `percentage spend on food and non-alcoholic drinks for lowest 20% by income`)  %>%
  filter(Year>2011) %>% 
  mutate(key = case_when(key=="percentage spend on food and non-alcoholic drinks for all households"~"all households",
                         key=="percentage spend on food and non-alcoholic drinks for middle 20% by income"~"middle 20% by income",
                         key=="percentage spend on food and non-alcoholic drinks for lowest 20% by income"~"lowest 20% by income"))



FSR_4_1_2plot <- ggplot(FSR_4_1_2) + 
  geom_line(aes(x=factor(Year), y=value, colour=key, group=key)) +
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20,2)) +
  scale_colour_manual(values = af_colours()) +
  labs(y = "% spend on food and non-alcoholic drinks",
       x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.direction = "vertical",
        legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = c(0,0)) 

FSR_4_1_2plot

save_graphic(FSR_4_1_2plot, '4.1.2','Average share of spend on food and non-alcoholic drinks, in low-income households and all households, in the UK') + 
  save_csv(FSR_4_1_2, '4.1.2','Average share of spend on food and non-alcoholic drinks, in low-income households and all households, in the UK')


-------------------------------------------------------------------------------------------------------------------------------------------------
  
# Support Graph 4: Share of final consumption expenditure of households G7 countries with OECD Data: Still need to apply AFColor as it got 7 variables.

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

F4_1_2d <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/Proportion of total household consumption expenditure spent on food and non-alcoholic beverages OECD Data.csv")

F4_1_2d <- F4_1_2d %>%
  gather(variable,value, `Canada`,`France`,`Germany`,`Italy`,`Japan`, `UK`, `US`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01"))) 

F4_1_2d_plot <- ggplot(F4_1_2d, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  labs(x = NULL,
       y = "Proportion of household expenditure (%)") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  #theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") 

F4_1_2d_plot

save_graphic(F4_1_2d_plot, '4.1.2d', ' Proportion of total household consumption expenditure spent on food and non-alcoholic beverages') + 
  save_csv(F4_1_2d, '4.1.2d', ' Proportion of total household consumption expenditure spent on food and non-alcoholic beverages')
