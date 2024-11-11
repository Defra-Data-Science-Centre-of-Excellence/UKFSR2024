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

# File didnt exist!
# t_4_1_4b <- aws.s3::s3read_using(FUN = read_csv,
#                                  bucket = ukfsr::s3_bucket(),
#                                  object = "theme_4/t4_1_2/output/csv/4_1_4b_num_healthy_start_vouchers_Eng_Wales_NI.csv")

t4_1_4b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/Healthy Start Data.csv") |> 
  filter(Region != "Unknown") |> 
  mutate(`% change` = (`Feb-24`/`Feb-22`*100)-100)

t4_1_4b$Region <- factor(t4_1_4b$Region, levels = c("East Midlands","East of England","London","North East","North West","South East","South West",
                                                    "West Midlands","Yorkshire and The Humber","Wales","Northern Ireland"))

level_order <- c("East Midlands","East of England","London","North East","North West","South East","South West","West Midlands","Yorkshire and The Humber",
                 "Wales","Northern Ireland")

af_colours_1 <- c(
  "#12436D" # Dark blue
)

t4_1_4b_plot <- ggplot(t4_1_4b, aes(x= Region, y=`% change`)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9), fill = af_colours_1) +
  scale_y_continuous(limits = c(-10,0), breaks=seq(-10,0,1)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14, horizontal = TRUE) +
  scale_fill_manual(values=af_colours_1) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  theme(axis.title.y = element_blank()) +
  labs(y = "Percentage change of beneficiaries receiving vouchers") +
  labs(x = "Types of concern\n") +
  geom_text(aes(label = round(`% change`,1)), size = 8, fontface = "bold", colour = "white", hjust = -0.5) +
  coord_flip() 

t4_1_4b_plot

save_graphic(t4_1_4b_plot, "4.1.4b", "num healthy start vouchers Eng Wales NI")
save_csv(t4_1_4b, "4.1.4b", "num healthy start vouchers Eng Wales NI")
