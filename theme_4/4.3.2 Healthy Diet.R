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

# % of disposable income -------------------------------------------------------

FSR_4_3_2d <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/4_3_2d_perc_dispos_income.csv")


FSR_4_3_2d <- FSR_4_3_2d %>%
  pivot_longer(cols = c(`2020-21`, `2021-22`, `2022-23`), 
               names_to = "Year", 
               values_to = "Value")

FSR_4_3_2d$Year <- factor(FSR_4_3_2d$Year, levels = unique(FSR_4_3_2d$Year))


# Plot the bar chart
FSR_4_3_2d_plot <- ggplot(FSR_4_3_2d, aes(x = `Income Quintile`, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Value), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, hjust = 0.5, 
            size = 7, color = 'black') + 
  scale_fill_manual(values = afcolours::af_colours(n= 3)) +
  labs(y = "% of disposable income ",
       x = "Income quintile",
       fill = "Type") +
  theme_ukfsr() 

FSR_4_3_2d_plot

save_graphic(FSR_4_3_2d_plot, '4.3.2d','Percentage of disposable income') + 
  save_csv(FSR_4_3_2d, '4.3.2d','Percentage of disposable income')
