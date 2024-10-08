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
                                    "London (inc Tilbury)", "Immingham", "Felixstowe",
                                    "Middlesbrough", "Belfast", "Hull", "Southampton",
                                    "Harwich International", "Other recorded ports", "No port recorded")))

# Create the plot
FSR_3_1_8_plot <- ggplot(FSR_3_1_8, aes(x = Port, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.9),  # Use position_dodge here
            size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  labs(y = "% of import", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(FSR_3_1_8_plot)

save_graphic(FSR_3_1_8_plot, '3.1.8', ' UK % of import by port') + 
  save_csv(FSR_3_1_8, '3.1.8', ' UK % of import by port')

--------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Import % by Short Straits from EU
  
FSR_3_1_8a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_3/input_data/Short_strait_improt_from_EU.csv")

# Convert data to long format
FSR_3_1_8a <- FSR_3_1_8a %>%
  gather(Year, Value, -Food)

FSR_3_1_8a <- FSR_3_1_8a %>%
  group_by(Year) %>%
  mutate(Food = fct_reorder(Food, Value, .desc = FALSE)) %>%
  ungroup()


# Create the plot
FSR_3_1_8a_plot <- ggplot(FSR_3_1_8a, aes(x = Food, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.9),  # Use position_dodge here
            size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  labs(y = "% of import", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(FSR_3_1_8a_plot)

save_graphic(FSR_3_1_8a_plot, '3.1.8a', 'Import % by short straits from EU') + 
  save_csv(FSR_3_1_8a, '3.1.8a', ' Import % by short straits from EU')

  
  
--------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Import % by Short Straits from EU + Non EU

FSR_3_1_8b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_3/input_data/Short_strait_improt_EU_&_NON-EU.csv")

# Convert data to long format
FSR_3_1_8b <- FSR_3_1_8b %>%
  gather(Year, Value, -Food)

FSR_3_1_8b <- FSR_3_1_8b %>%
  group_by(Year) %>%
  mutate(
    # Get the order of Food based on Value
    Food_order = if_else(Food == "Food, feed & drink",
                         0,  # Assign the highest priority to "Food, feed & drink"
                         rank(-Value, ties.method = "first")),  # Rank other foods
    Food = fct_reorder(Food, rev(Food_order))
  ) %>%
  ungroup()


# Create the plot
FSR_3_1_8b_plot <- ggplot(FSR_3_1_8, aes(x = Food, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.9),  # Use position_dodge here
            size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  labs(y = "% of import", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(FSR_3_1_8b_plot)

save_graphic(FSR_3_1_8b_plot, '3.1.8b', ' Import % by short straits from EU + Non EU') + 
  save_csv(FSR_3_1_8b, '3.1.8b', ' Import % by short straits from EU + Non EU')
