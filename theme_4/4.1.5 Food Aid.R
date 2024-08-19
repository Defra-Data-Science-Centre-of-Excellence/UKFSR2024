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
                                  object = "theme_4/input_data/Household food bank usage.csv")


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

save_graphic(FSR_4_1_5_plot, '4.1.5','% of households who have used a food bank') + 
  save_csv(FSR_4_1_5, '4.1.5','% of households who have used a food bank')


--------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Support 1 - Percentage of households using supermarket
  
FSR_4_1_5a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Percentage of households using supermarket.csv")


FSR_4_1_5a$`Frequency of social supermarket use` <- factor(FSR_4_1_5a$`Frequency of social supermarket use`,
                                                           levels = rev(c('Everyday or most days',
                                                                      '2-3 times a week / about once a week',
                                                                      '2-3 times a month / about once a month',
                                                                      'Less than once a month',
                                                                      'Can\'t remember')))

source(here::here("utils", "load-font.R"))

FSR_4_1_5a_plot <- ggplot(FSR_4_1_5a, aes(x = `Frequency of social supermarket use`, y = `% of respondents`)) +
  geom_bar(stat = "identity", width = 0.5, fill = af_colours("categorical")[1]) +
  geom_text(aes(label = `% of respondents`), 
            position = position_dodge(width = 0.5), 
            vjust = 0.5,
            hjust = -0.2, 
            colour= "black",
            family = "GDS Transport Website",
            size = 10) +
  labs(y = "% of respondents", x = "Frequency of social supermarket use") +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website",
              horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())

FSR_4_1_5a_plot

save_graphic(FSR_4_1_5a_plot, '4.1.5a','Percentage of households using supermarket') + 
  save_csv(FSR_4_1_5a, '4.1.5a','Percentage of households using supermarket')
