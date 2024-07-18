library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('scales')
library('zoo')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")



FSR_4_1_7 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/Digital access to food shop dataset.csv",
                                  col_names = TRUE, 
                                  skip = 5)

colnames(FSR_4_1_7) <- c("Year", "All retailing", "Food stores")
FSR_4_1_7 <- FSR_4_1_7[, c(1:3)]

FSR_4_1_7 <- FSR_4_1_7 %>%
  gather(variable,value, `All retailing`,`Food stores`) %>%
  mutate(Year = as.Date(as.yearmon(Year, "%Y %b")))


FSR_4_1_7_plot <- ggplot(FSR_4_1_7, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),date_labels = "%Y") +  # Corrected `date_labels`
  labs(x = NULL,
       y = "Percent (%)") +
  theme_ukfsr(base_family = "GDS Transport Website") 

FSR_4_1_7_plot


save_graphic(FSR_4_1_7_plot, '4.1.7', ' Internet Sales as a propotion of all retailing ') + 
  save_csv(FSR_4_1_7, '4.1.7', 'Internet Sales as a propotion of all retailing')