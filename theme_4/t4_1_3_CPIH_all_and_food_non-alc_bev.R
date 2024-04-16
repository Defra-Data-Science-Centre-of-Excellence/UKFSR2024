library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(tidyverse)
library(scales)
library(purrr)
library(lubridate)
library(countrycode)
library(mm23)

source(here("utils", "load-font.R"))
source(here("utils", "helpers-cpi.R"))

# download data ----------------------------------------------------------------
mm23 <- mm23::acquire_mm23()

metadata <- get_mm23_metadata(mm23)
mm23_month <- get_mm23_month(mm23)
weights <- get_weights(mm23, measure = "cpi")

cpih_all_yy <- "L55O"
cpih_food_yy <- "L55P"
cpih_all_mm <- "L59C"
cpih_food_mm <- "L59D"

cdids <- c(cpih_all_yy, cpih_food_yy)

af_duo_colours <- afcolours::af_colours("duo")

cht <- line_chart(mm23_month, cdids)
t4_1_3_plot <- cht +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(y = "CPIH value") +
  scale_colour_manual(values=af_duo_colours, labels = c("Overall inflation", "Food inflation")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.text = element_text(margin = margin(r = 2, unit = 'cm')))

save_graphic(t4_1_3_plot,"4.1.3","cpih all and food non-alc bev")

# filter for relevant entries
t4_1_3 <- mm23_month |> 
filter(cdid == "L55O" | cdid == "L55P") 

# rename cells
t4_1_3$cdid[t4_1_3$cdid == "L55O"] <- "Overall inflation"
t4_1_3$cdid[t4_1_3$cdid == "L55P"] <- "Food inflation"

# deleted unwanted column
t4_1_3 <- t4_1_3[ -c(4) ]

# write out data to .CSV (and store in the bucket)
save_csv(t4_1_3,"4.1.3","cpih all and food non-alc bev") 


  
  
  
