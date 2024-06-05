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

t5_1_2a <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_1_2/output/csv/5_1_2a_FSA_respond_ten_most_common_prompted_concerns.csv",
                                col_types = colspec)

t5_1_2a$`Types of concern` <- factor(t5_1_2a$`Types of concern`, levels = c("Food prices","Food waste","The amount of food packaging","The quality of food",
                                                                            "The amount of sugar in food","Food hygiene when ordering takeaways","Animal welfare",
                                                                            "Being able to eat healthily","Food hygiene when eating out","The amount of fat in food",
                                                                            "The amount of calories in food","The use of additives","Hormones, steroids or antibiotics in food"))

level_order <- c("Food prices","Food waste","The amount of food packaging","The quality of food",
                 "The amount of sugar in food","Food hygiene when ordering takeaways","Animal welfare",
                 "Being able to eat healthily","Food hygiene when eating out","The amount of fat in food",
                 "The amount of calories in food","The use of additives","Hormones, steroids or antibiotics in food")

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t5_1_2a_plot <- ggplot(t5_1_2a, aes(x= reorder(`Types of concern`, `Percentage of respondents`), y=`Percentage of respondents`)) +
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

t5_1_2a_plot

save_graphic(t5_1_2a_plot, "5.1.2a", "fsa respond ten most common prompted concerns")

save_csv(t5_1_2a, "5.1.2a", "fsa respond ten most common prompted concerns")
