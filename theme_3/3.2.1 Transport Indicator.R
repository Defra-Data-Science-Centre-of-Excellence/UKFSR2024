#devtools::install_github("FoodchainStats/ukfsr")


# Load required libraries
library(sf)
library(ukfsr)
library(afcolours)
library(ggplot2)
library(dplyr)
library(tidyr)
library(aws.s3)
library(aws.ec2metadata)
library(lubridate)

source(here::here("utils", "load-font.R"))

# SRN Average delay map --------------------------------------------------------

uk_border <- aws.s3::s3read_using(FUN = sf::st_read,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/Transport Delay Data File/CTRY_DEC_2022_GB_BUC.geojson")


shp <- aws.s3::s3read_using(FUN = sf::st_read,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/Transport Delay Data File/2022_Delay_v2.geojson")



# Define fill colors based on country name
fill_colors <- c("England" = "white", "Scotland" = "grey70", "Wales" = "grey70")


# Define delay ranges and labels
delay_breaks <- c(0, 5, 10, 15, 20, Inf)
delay_labels <- c("Less than 5 seconds", "5 - 10 seconds", "10 - 15 seconds", "15 - 20 seconds", "More than 20 seconds")

# Get AFCOLOUR palette for categorical data
af_colors <- af_colours(type = "categorical", n = length(delay_breaks) - 1) # Get 5 colors

# Create the map plot
map_plot <- ggplot() +
              # Add UK border as the base layer
              geom_sf(data = uk_border |> filter(CTRY22NM == "England"), aes(fill = CTRY22NM), color = "black", size = 0.5) +
              scale_fill_manual(values = fill_colors) +
              guides(fill = FALSE) +
              
              # Add transport delay data with color encoding on top
              geom_sf(data = shp,
                      aes(color = factor(cut(AvgDelay, breaks = delay_breaks, labels = delay_labels, include.lowest = TRUE))),
                      linewidth = 1) +
              
              #labs(title = "2022 Transport Delay (seconds/vehicle/mile)") +
              
              scale_color_manual(values = c(af_colors),
                                 labels = delay_labels,
                                 name = "Avg Delay\n(seconds/vehicle/mile)",
                                 drop = FALSE) + 
              theme_ukfsr(base_family = "GDS Transport Website") +
              theme(axis.text.x = element_blank(), 
                    axis.text.y = element_blank(),
                    axis.line.x = element_blank(),axis.ticks.x = element_blank(),
                    panel.grid = element_blank(),
                    legend.direction = "vertical",
                    legend.position = "inside", legend.position.inside = c(0.1, 0.5),
                    legend.text = element_text(size = 22), legend.title = element_text(size = 22)) 
              

map_plot  

# Output the plot
# save_graphic(map_plot, '3.2.4a','Road Congestion and Travel Time Statistics (average speed and delay)') 
#+ save_csv(FSR_3_1_7, '3.1.7','Road Congestion and Travel Time Statistics (average speed and delay)')

# SIMPLIFIED VERSION TO REDUCE FILESIZE

shp2 <- shp |> 
  mutate(colour = factor(cut(AvgDelay, breaks = delay_breaks, labels = delay_labels, include.lowest = TRUE))) |> 
  group_by(colour) |> 
  summarise(geometry = st_union(geometry)) |> st_simplify(dTolerance = 1000)


map_plot <- ggplot() +
  # Add UK border as the base layer
  geom_sf(data = uk_border |> filter(CTRY22NM == "England"), aes(fill = CTRY22NM), color = "black", size = 0.5) +
  scale_fill_manual(values = fill_colors) +
  guides(fill = FALSE) +
  
  # Add transport delay data with color encoding on top
  geom_sf(data = shp2,
          aes(color = colour),
          linewidth = 1) +
  scale_color_manual(values = c(af_colors),
                     labels = delay_labels,
                     name = "Avg Delay\n(seconds/vehicle/mile)",
                     drop = FALSE) + 
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.line.x = element_blank(),axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.direction = "vertical",
        legend.position = "inside", legend.position.inside = c(0.1, 0.5),
        legend.text = element_text(size = 24), legend.title = element_text(size = 24)) 


map_plot 

# Output the plot
save_graphic(map_plot, '3.2.1b','Road Congestion and Travel Time Statistics (average speed and delay)') 


# SRN Average delay chart-------------------------------------------------------
  
delay <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_3/input_data/3_1_3c_ave_delay_Strategic_Road_Network.csv")

delay <- delay %>%
  gather(variable,value, `Monthly`,`Year ending`)


delay$Month <- lubridate::dmy(paste("01", delay$Month))

print(delay)



# Create the plot
delay_plot <- ggplot(delay, aes(x = Month, y = value, colour = variable, group = variable, label = variable)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  
  # Set scale_x_date with breaks every 6 months
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y") +
  
  scale_colour_manual(values = af_colours(type = c("duo"), n = 2)) +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0)) +
  theme(axis.title.x = element_blank()) +
  
  labs(y = "Average delay (seconds per vehicle per mile)") +
  
  geom_line(size = 2) +
  
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1)) +
  theme(axis.text.y = element_text(size = 16)) +
  theme(legend.text = element_text(size = 22)) +
  guides(colour = guide_legend(override.aes = list(size = 1)))

delay_plot <- 
delay |> 
  ggplot() +
  geom_line(aes(x = Month, y = value, colour = variable, group = variable, label = variable)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.05))) +
  scale_colour_manual(values = af_colours(type = c("duo"), n = 2)) +
  labs(x = NULL, y = "Average delay (seconds per vehicle per mile)") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1))
  
  

# Output the plot
save_graphic(delay_plot, '3.2.1a','Average Delay (second per vehicle)')
save_csv(delay, '3.2.1a','Average Delay (second per vehicle)')
