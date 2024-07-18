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

FSR_4_1_9 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_4/input_data/4.1.9 Percentage spend on food (non-Free School Meals support in the holidays).csv")


# Renaming columns to more suitable names
colnames(FSR_4_1_9) <- c("Region", "Regional_Code", "PercentSpend")


FSR_4_1_9$PercentSpend <- as.numeric(sub("%", "", FSR_4_1_9$PercentSpend))


# Ordering the Region factor by RegionalCode
FSR_4_1_9 <- FSR_4_1_9 %>%
  arrange(Regional_Code) %>%
  mutate(Region = factor(Region, levels = Region))


FSR_4_1_9_plot <- ggplot(FSR_4_1_9, aes(x = Region, y = PercentSpend))  +
  geom_bar(stat = "identity", fill = af_colours(n=1)) +
  geom_text(aes(label = PercentSpend), vjust = 2, color = "white", size = 5) +
  scale_y_continuous(breaks = seq(0, max(FSR_4_1_9$PercentSpend), by = 5)) +
  guides(fill = guide_legend(byrow = TRUE)) +
  #scale_fill_manual(values=af_colours(), guide = guide_legend(reverse = TRUE)) +
  labs(x = NULL,
       y = "Average % spend on food ( Non- Free School Meals in the holidays)") +
  scale_x_discrete(labels = wrap_format(3)) + 
  theme_ukfsr(base_family = "GDS Transport Website") 


FSR_4_1_9_plot

save_graphic(FSR_4_1_9_plot, '4.1.9','Percentage spend on Food (Non-School Meals Support in Holidays)') + 
  save_csv(FSR_4_1_9, '4.1.9','Percentage spend on Food (Non-School Meals Support in Holidays)')
