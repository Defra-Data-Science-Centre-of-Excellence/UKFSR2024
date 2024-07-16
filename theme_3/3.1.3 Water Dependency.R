library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_3_1_3a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/proportion of spray irrigation volume licensed.csv")



FSR_3_1_3a <- FSR_3_1_3a %>%
  pivot_longer(cols = starts_with("Indicative proportion"),
               names_to = "Year",
               values_to = "Proportion") %>%
  mutate(Year = ifelse(str_detect(Year, "2022"), "2022", "2023"))

# Plot the bar chart
FSR_3_1_3a_plot <- ggplot(FSR_3_1_3a, aes(x = `Former EA Region`, y = Proportion, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = afcolours::af_colours("duo"), labels = c("2022", "2023")) +
  labs(y = "Proportion licensed for storage (%)",
       x = NULL,
       fill = "Year") +
  theme_ukfsr() 

FSR_3_1_3a_plot

save_graphic(FSR_3_1_3a_plot, '3.1.3a', 'Indicative proportion of spray irrigation licensed for storage') + 
  save_csv(FSR_3_1_3a, '3.1.3a', 'Indicative proportion of spray irrigation licensed for storage')