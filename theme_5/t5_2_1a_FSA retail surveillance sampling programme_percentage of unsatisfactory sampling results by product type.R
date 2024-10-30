library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(scales)
library(data.table)

source(here("utils", "load-font.R"))

t5_2_1a <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_2_1/output/csv/5_2_1a_FSA_retail_surveillance_sampling_programme_percentage_of_unsatisfactory_sampling_results_by_product_type.csv")

t5_2_1a$Product <- factor(t5_2_1a$Product, levels = c("Bread","Sausages*","Butter*","Yoghurts*","Chicken Ready Meals*",
                                                  "Olive oil","Orange Juice","Pasta*","Oregano","Free From","Cheese",
                                                  "Vegan products","Coffee*","Turmeric","Breakfast Cereals*","Milk*",
                                                  "Basmati Rice*","Minced meat*"))

t5_2_1a_long <- t5_2_1a |> 
  group_by(Product) |>
  pivot_longer(cols=c("2021/2022","2022/2023","2023/2024"),
               names_to="Year",
               values_to="value")

t5_2_1a$Year <- factor(t5_2_1a$Year, levels = c("2021/2022","2022/2023","2023/2024"))

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(t5_2_1a_long$Year)

# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/
# https://stackoverflow.com/questions/11366964/is-there-a-way-to-change-the-spacing-between-legend-items-in-ggplot2
t5_2_1a_plot <- ggplot(t5_2_1a_long, aes(x=factor(Product), y=value, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge()) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours, 
                    labels=c("2021/22          ",
                             "2022/23          ",
                             "2023/24")) +
  labs(x = "Commodities") +
  labs(y = "Percentage of unsatisfactory\nsamples") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "horizontal", legend.position = "bottom", legend.box = "horizontal") 

t5_2_1a_plot


save_graphic(t5_2_1a_plot, "5.2.1a", "fsa retail surveillance sampling programme_percentage of unsatisfactory sampling results by product type")

save_csv(t5_2_1a, "5.2.1a", "fsa retail surveillance sampling programme_percentage of unsatisfactory sampling results by product type")
