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
                                  object = "theme_4/input_data/UK consumption of different food groups.csv")



         
# Reorder the Food_group factor by the Percentage_change
FSR_4_1_8 <- FSR_4_1_8 %>%
  mutate(`Food group` = factor(`Food group`,  levels = FSR_4_1_8$`Food group`[order(FSR_4_1_8$`% change`)]))

# Create the bar plot
FSR_4_1_8_plot <- ggplot(FSR_4_1_8, aes(x = `Food group`, y = `% change`, fill = `% change`)) +
  geom_bar(stat = "identity", fill = af_colours()[1]) +
  geom_text(aes(label = round(`% change`, 1), 
                hjust = ifelse(`% change` < 0, 1.2, -0.2)),  # Adjust hjust based on positive/negative values
            color = "black",
            size = 7) +
  coord_flip() +  # Flip coordinates to have horizontal bars
  labs(y = "% Change since FYE 2020 to FYE 2022", x = NULL) +
  theme_ukfsr()

# Print the plot
FSR_4_1_8_plot

save_graphic(FSR_4_1_9_plot, '4.1.8','Change in UK consumption of different food groups eaten in the home') + 
  save_csv(FSR_4_1_9, '4.1.8','Change in UK consumption of different food groups eaten in the home')