library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)

source(here("utils", "load-font.R"))

t5_1_1h <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_5/t_5_1_1/output/csv/5_1_1h_fss_respond_confid_food_supply_chain_actors.csv",
                                 col_types = colspec)

t5_1_1h$`Confident in the part of the food supply chain` <- factor(t5_1_1h$`Confident in the part of the food supply chain`, levels=unique(t5_1_1h$`Confident in the part of the food supply chain`))

af_colours_1 <- c(
  "#12436D" # Dark blue
)

level_order <- c("Ensure that food is of a high quality","Ensure that food is safe to eat")

t5_1_1h_plot <- ggplot(t5_1_1h, aes(x= factor(`Confident in the part of the food supply chain`, level = level_order), y=`Percentage of respondents`)) +
  geom_bar(stat="identity", fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website",horizontal = TRUE) +
  scale_fill_manual(values=gcols) +
  labs(y = "Percentage of respondents (%)") +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    axis.title.x= element_text(size=20),
    axis.title.y = element_blank(),
    text = element_text(family = "GDS Transport Website")) +
  geom_text(aes(label = round(`Percentage of respondents`,0)), vjust = 0.5, hjust = 2, size = 8, fontface = "bold", colour = "white") +
  coord_flip() 

t5_1_1h_plot

save_graphic(t5_1_1h_plot, "5.1.1h", "FSS respond confid food supply chain actors")
