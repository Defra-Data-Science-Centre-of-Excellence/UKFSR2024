library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('scales')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

# Food bank usage graph

FSR_4_1_5 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Household food security status.csv")


FSR_4_1_5 <- FSR_4_1_5 %>%
  pivot_longer(cols = c(`30 day food bank usage`, `12 month food bank usage`), 
               names_to = "Usage_Period", 
               values_to = "Usage_Value")

FSR_4_1_5$Usage_Period <- factor(FSR_4_1_5$Usage_Period, levels = unique(FSR_4_1_5$Usage_Period))



# Plot the bar chart
FSR_4_1_5_plot <- ggplot(FSR_4_1_5, aes(x = `Household food security status`, y = Usage_Value, fill = Usage_Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = afcolours::af_colours("duo")) +
  labs(y = "% of households",
       x = NULL,
       fill = "Type") +
  theme_ukfsr() 

FSR_4_1_5_plot

save_graphic(FSR_4_1_5_plot, '4.1.5','% of Household foodbank usage security status') + 
  save_csv(FSR_4_1_5, '4.1.5','% of Household foodbank usage security status')