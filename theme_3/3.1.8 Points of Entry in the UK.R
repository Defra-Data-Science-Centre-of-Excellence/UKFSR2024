library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

FSR_3_1_8 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/UK Import data.csv")

# Convert data to long format
FSR_3_1_8 <- FSR_3_1_8 %>%
  gather(Year, Value, -Port)

# Convert 'Port' to a factor with the specified order
FSR_3_1_8$Port <- factor(FSR_3_1_8$Port, 
                         levels = rev(c("Dover/Folkestone/Eurotunnel", "Liverpool", "London Gateway",
                                    "London(inc Tilbury)", "Immingham", "Felixstowe",
                                    "Middlesbrough", "Belfast", "Hull", "Southampton",
                                    "Harwich International", "Other recorded ports", "No port recorded")))

# Create the plot
FSR_3_1_8_plot <- ggplot(FSR_3_1_8, aes(x = Port, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "% of import", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(FSR_3_1_8_plot)

save_graphic(FSR_3_1_8_plot, '3.1.8', ' UK % of import by port') + 
  save_csv(FSR_3_1_8, '3.1.8', ' UK % of import by port')