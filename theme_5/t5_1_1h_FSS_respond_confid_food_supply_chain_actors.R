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

t5_1_1h$confid_wrap <- str_wrap(t5_1_1h$`Confident in the part of the food supply chain`, width = 14)

t5_1_1h_plot <- ggplot(t5_1_1h, aes(x= factor(confid_wrap), y=`Percentage of respondents`)) +
  geom_bar(stat="identity", width=0.7, fill = af_colours_1) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_ukfsr(base_family = "GDS Transport Website",horizontal = TRUE) +
  scale_fill_manual(values=af_colours_1) +
  labs(y = "Percentage of respondents (%)") +
  theme(
    axis.title.y = element_blank()) + 
  geom_text(aes(label = round(`Percentage of respondents`,0)), vjust = 0.5, hjust = 2, size = 8, fontface = "bold", colour = "white") +
  coord_flip() 

t5_1_1h_plot

save_graphic(t5_1_1h_plot, "5.1.1h", "FSS respond confid food supply chain actors")
