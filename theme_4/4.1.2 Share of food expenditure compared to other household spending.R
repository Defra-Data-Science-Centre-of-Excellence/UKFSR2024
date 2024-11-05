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
library(cowplot)
library(here)
library(sf)
library(janitor)


contents <- get_bucket_df("s3-ranch-054")

source(here::here("utils", "load-font.R"))

F4_1_2 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                 bucket = "s3-ranch-054",
                                 object = "theme_4/input_data/4_1_2a_ave_spend_food_non_alcohol_drinks_by_income.csv")


FSR_4_1_2 <- F4_1_2 %>% 
  gather(key,value, `percentage spend on food and non-alcoholic drinks for all households`,`percentage spend on food and non-alcoholic drinks for middle 20% by income`, `percentage spend on food and non-alcoholic drinks for lowest 20% by income`,`percentage spend on food and non-alcoholic drinks for highest 20% by income`)  %>%
  mutate(key = case_when(key=="percentage spend on food and non-alcoholic drinks for all households"~"All households",
                         key=="percentage spend on food and non-alcoholic drinks for middle 20% by income"~"Middle 20% by income",
                         key=="percentage spend on food and non-alcoholic drinks for lowest 20% by income"~"Lowest 20% by income",
                         key=="percentage spend on food and non-alcoholic drinks for highest 20% by income"~"Highest 20% by income")) %>% 
  mutate(key = factor(key, levels = c("Lowest 20% by income","Middle 20% by income","All households","Highest 20% by income"), ordered = TRUE))



FSR_4_1_2plot <- ggplot(FSR_4_1_2) + 
  geom_line(aes(x=factor(Year), y=value, colour=key, group=key)) +
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20,2)) +
  scale_x_discrete(breaks = unique(FSR_4_1_2$Year)[c(T,F,F)])+
  scale_colour_manual(values = af_colours()) +
  labs(y = "% spend on food and non-alcoholic drinks",
       x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.direction = "vertical",
        legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = c(0,0)) 


FSR_4_1_2plot

save_graphic(FSR_4_1_2plot, '4.1.2','Average share of spend on food and non-alcoholic drinks, by household income, in the UK') + 
  save_csv(FSR_4_1_2, '4.1.2','Average share of spend on food and non-alcoholic drinks, by household income, in the UK')


--------------------------------------------------------------------------------------------------------------------------------------------------------
#Household income before housing costs by quintiles
  
F4_1_2b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                 bucket = "s3-ranch-054",
                                 object = "theme_4/input_data/4_1_2b_household_income_in_the_UK.csv")

FSR_4_1_2b <- F4_1_2b %>% 
  gather(key,value, `Quintile 1`,`Quintile 3 (median)`, `Quintile 5`)  %>%
  mutate(key = factor(key, levels = c("Quintile 5","Quintile 3 (median)", "Quintile 1"), ordered = TRUE))

FSR_4_1_2b_plot <- ggplot(FSR_4_1_2b) + 
  geom_line(aes(x=factor(Year), y=value, colour=key, group=key)) +
  scale_y_continuous(limits = c(0,1250), breaks=seq(0,1250,250)) +
  scale_x_discrete(breaks = unique(FSR_4_1_2b$Year)[c(T,F,F,F)])+
  scale_colour_manual(values = af_colours()) +
  labs(y = str_wrap("Household income before housing costs (£ per week equivalised)", width = 35),
       x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(axis.text.x = element_text(angle = 45,vjust= 0.5)) +
  theme(legend.direction = "vertical",
        legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = c(0,0)) +
  theme(plot.margin = margin(t = 10,  # Top margin
                             r = 40,  # Right margin
                             b = 10,  # Bottom margin
                             l = 5))  # Left margin

FSR_4_1_2b_plot  

save_graphic(FSR_4_1_2b_plot, '4.1.2b','Household income before housing costs in the UK') + 
  save_csv(FSR_4_1_2b, '4.1.2b','Household income before housing costs in the UK')

------------------------------------------------------------------------------------------------------------------------
#Actual average weekly household expenditure (ONS Family Spending in the UK)
  
F4_1_2c <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_4/input_data/Actual average weekly household expenditure.csv")

F4_1_2c <- F4_1_2c %>%
  gather(variable, value, `Food & non-alcoholic drinks`, `Restaurants & hotels`, `Other`) %>%
  filter(Year %in% c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23"))

F4_1_2c <- F4_1_2c %>%
  mutate(variable = factor(variable, levels = rev(c("Food & non-alcoholic drinks", "Restaurants & hotels", "Other"))))



F4_1_2c_plot <- ggplot(F4_1_2c, aes(x = Year, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = value), 
            position = position_stack(vjust = 0.5), 
            vjust = 0.5, 
            hjust = 0.3, 
            colour = "white", 
            family = "GDS Transport Website", 
            size = 7) +
  scale_fill_manual(values = af_colours()) +  
 # Only label these years
  labs(x = NULL,
       y = "Average weekly household expenditure (£)") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill = guide_legend(byrow = TRUE, reverse = TRUE)) 

F4_1_2c_plot

save_graphic(F4_1_2c_plot, '4.1.2c', 'Actual average weekly household expenditure') + 
  save_csv(F4_1_2c, '4.1.2c', 'Actual average weekly household expenditure')




-------------------------------------------------------------------------------------------------------------------------------------------------
  
# Support Graph 4: Share of final consumption expenditure of households G7 countries with OECD Data: Still need to apply AFColor as it got 7 variables.

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

F4_1_2d <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/Proportion of total household consumption expenditure spent on food and non-alcoholic beverages OECD Data.csv")

variable_order <- c("US", "Canada", "Japan", 
                    "France", "Germany", "Italy", 
                    "UK")

F4_1_2d <- F4_1_2d %>%
  gather(variable,value, `Canada`,`France`,`Germany`,`Italy`,`Japan`, `UK`, `US`) %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01")),
         variable = factor(variable, levels = variable_order)) 

af_colours_2 <- function() {c("US" = "#12436D", "Canada" = "#12436D", "Japan" = "#12436D",
                           "France" = "#12436D", "Germany" = "#12436D", "Italy" = "#12436D",
                           "UK" = "#F46A25" )}

F4_1_2d_plot <- ggplot(F4_1_2d, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  facet_wrap(~ variable) +
  scale_colour_manual(values = af_colours_2())+
  scale_y_continuous(breaks = seq(from = 0, to = 17, by = 4), limits = c(0,17)) +
  labs(x = NULL,
       y = "Proportion of household expenditure (%)") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(legend.position = "none") +
  #theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") 

F4_1_2d_plot

save_graphic(F4_1_2d_plot, '4.1.2d', ' Proportion of total household consumption expenditure spent on food and non-alcoholic beverages') + 
  save_csv(F4_1_2d, '4.1.2d', ' Proportion of total household consumption expenditure spent on food and non-alcoholic beverages')


