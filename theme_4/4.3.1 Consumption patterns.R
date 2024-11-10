# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('scales')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")


FSR_4_1_8 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/UK_food_consumption_updated.csv")



         
# Reorder the Food_group factor by the Percentage_change
FSR_4_1_8 <- FSR_4_1_8 %>%
  mutate(`Food group` = factor(`Food group`,  levels = FSR_4_1_8$`Food group`[order(FSR_4_1_8$`% change since FYE 2020`)]))

# Create the bar plot
FSR_4_1_8_plot <- ggplot(FSR_4_1_8, aes(x = `Food group`, y = `% change since FYE 2020`, fill = `% change since FYE 2020`)) +
  geom_bar(stat = "identity", fill = af_colours()[1]) +
  geom_text(aes(label = round(`% change since FYE 2020`, 1), 
                hjust = ifelse(`% change since FYE 2020` < 0, 1.2, -0.2)),  # Adjust hjust based on positive/negative values
            color = "black",
            size = 7, family = "GDS Transport Website") +
  coord_flip() +  # Flip coordinates to have horizontal bars
  labs(y = "% Change from FYE 2020 to FYE 2023", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE)+
  scale_y_continuous(limits = c(-18,0), breaks = seq(-18,0,by =2))

# Print the plot
FSR_4_1_8_plot

save_graphic(FSR_4_1_8_plot, '4.3.1a','Change in UK consumption of different food groups eaten in the home') 
  save_csv(FSR_4_1_8, '4.3.1a','Change in UK consumption of different food groups eaten in the home')
