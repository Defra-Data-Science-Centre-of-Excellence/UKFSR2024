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

FSR_4_1_4 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Healthy Start Data.csv")

FSR_4_1_4 <- FSR_4_1_4 %>%
  arrange(`Region Code`) %>%
  mutate(Region = factor(Region,  levels = rev(unique(Region))))




FSR_4_1_4 <- FSR_4_1_4 %>%
  pivot_longer(cols = c(`Feb-22`, `Feb-24`), 
               names_to = "Month", 
               values_to = "Value")

FSR_4_1_4$Month <- factor(FSR_4_1_4$Month, levels = unique(FSR_4_1_4$Month))

FSR_4_1_4$Value <- round(FSR_4_1_4$Value/1000,1)

# Plot the bar chart
FSR_4_1_4_plot <- ggplot(FSR_4_1_4, aes(x = `Region`, y = Value, fill = Month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Value), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5, hjust = -0.1, 
            size = 6, color = 'black') + 
  coord_flip() + 
  scale_fill_manual(values = afcolours::af_colours("duo")) +
  labs(y = "Number of beneficiaries in thousands ",
       x = NULL,
       fill = "Type") +
  theme_ukfsr() 

FSR_4_1_4_plot

save_graphic(FSR_4_1_4_plot, '4.1.4','Healthy Start Voucher') + 
  save_csv(FSR_4_1_4, '4.1.4','Healthy Start Voucher')