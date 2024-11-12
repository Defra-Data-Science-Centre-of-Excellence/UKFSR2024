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

source(here("utils", "load-font.R"))

t5_1_2c <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_1_2/output/csv/5_1_2c_FSS_respond_ten_most_common_prompted_concerns.csv",
                                col_types = colspec)

t5_1_2c$`Types of concern` <- factor(t5_1_2c$`Types of concern`, levels = c("Food prices","Food poverty and food inequality","Ultra-processed, or over-processing of food",
                                                                            "The safety of food imported from outside the UK","The 'healthiness' of people's diets in general",
                                                                            "The sustainability of food and food production","The quality of food imported from outside the UK",
                                                                            "Animal welfare in the food industry","Ingredients and additives in food","Genetically modified (GM) food",
                                                                            "Food availability/food shortages","The safety of food produced in the UK",
                                                                            "The information on food labels being accurate","The quality of food produced in the UK"))

level_order <- c("Food prices","Food poverty and food inequality","Ultra-processed, or over-processing of food",
                 "The safety of food imported from outside the UK","The 'healthiness' of people's diets in general",
                 "The sustainability of food and food production","The quality of food imported from outside the UK",
                 "Animal welfare in the food industry","Ingredients and additives in food","Genetically modified (GM) food",
                 "Food availability/food shortages","The safety of food produced in the UK",
                 "The information on food labels being accurate","The quality of food produced in the UK")

t5_1_2c_plot <- ggplot(t5_1_2c, aes(x= reorder(`Types of concern`, `Percentage of respondents`), y=`Percentage of respondents`)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14, horizontal = TRUE) +
  scale_fill_manual(values=af_colours_1) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  labs(y = "Percentage of respondents (%)") +
  labs(x = "Types of concern\n") +
  geom_text(aes(label = round(`Percentage of respondents`,0)), vjust = 0.5, hjust = 2, size = 8, fontface = "bold", colour = "white") +
  coord_flip() 

t5_1_2c_plot

save_graphic(t5_1_2c_plot, "5.1.2c", "fss respond ten most common prompted concerns")

save_csv(t5_1_2c, "5.1.2c", "fss respond ten most common prompted concerns")
