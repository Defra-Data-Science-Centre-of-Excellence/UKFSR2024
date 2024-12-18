library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(lubridate)
library(forcats)

source(here::here("utils", "load-font.R"))

# Import % by port -------------------------------------------------------------

ports <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/UK Import data.csv")

# Convert data to long format
ports <- ports %>%
  gather(Year, Value, -Port)

# Convert 'Port' to a factor with the specified order
ports$Port <- factor(ports$Port,
                         levels = rev(c("Dover/Folkestone/Eurotunnel", "Liverpool", "London Gateway",
                                    "London (inc Tilbury)", "Immingham", "Felixstowe",
                                    "Middlesbrough", "Belfast", "Hull", "Southampton",
                                    "Harwich International", "Other recorded ports", "No port recorded")))

# Create the plot
ports_plot <- ggplot(ports, aes(x = Port, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = round(Value, 1)), 
  #           position = position_dodge(width = 0.9),  # Use position_dodge here
  #           size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  labs(y = "percent of FFD imports", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.margin = margin(r = 0.5, unit = "cm"))

# Print the plot
print(ports_plot)

save_graphic(ports_plot, '3.2.2a', ' UK pct of import by port') 
save_csv(ports, '3.2.2a', ' UK pct of import by port')

  
# NOT USED Import % by Short Straits from EU -----------------------------------
  
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
  # geom_text(aes(label = round(Value, 1)), 
  #           position = position_dodge(width = 0.9),  # Use position_dodge here
  #           size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(y = "percent of FFD imports", x = NULL, fill = "Year") +
  scale_fill_manual(values = af_colours('duo')) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(FSR_3_1_8a_plot)

# save_graphic(FSR_3_1_8a_plot, '3.3.1d', 'Import pct by product from short straits from EU') 
# save_csv(FSR_3_1_8a, '3.3.1d', ' Import pct by product from short straits from EU')

  
  
# Import % by Short Straits from EU + Non EU -----------------------------------

ss_products <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_3/input_data/Short_strait_improt_EU_&_NON-EU.csv")

# Convert data to long format
ss_products <- ss_products %>%
  gather(Year, Value, -Food)

ss_products <- ss_products %>%
  group_by(Year) %>%
  filter(Food != "Food, feed & drink") |> 
  mutate(
    # Get the order of Food based on Value
    Food_order = if_else(Food == "Food, feed & drink",
                         0,  # Assign the highest priority to "Food, feed & drink"
                         rank(-Value, ties.method = "first")),  # Rank other foods
    Food = fct_reorder(Food, rev(Food_order))
  ) %>%
  ungroup()


# Create the plot
ss_products_plot <- ggplot(ss_products, aes(x = Food, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = round(Value, 1)), 
  #           position = position_dodge(width = 0.9),  # Use position_dodge here
  #           size = 7, color = "black", vjust = 0.6, hjust = -0.2) +  
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_fill_manual(values = af_colours('duo')) +
  labs(y = "percent of FFD imports", x = NULL, fill = "Year") +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())

# Print the plot
print(ss_products_plot)

save_graphic(ss_products_plot, '3.2.2b', 'Import pct by product from short straits from EU + Non EU') 
save_csv(ss_products, '3.2.2b', 'Import pct by product from short straits from EU + Non EU')
