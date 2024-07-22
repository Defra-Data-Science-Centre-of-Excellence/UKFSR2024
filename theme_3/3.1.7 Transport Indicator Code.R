# Load required libraries
library('sf')
library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('aws.ec2metadata')
library('lubridate')



contents <- get_bucket_df("s3-ranch-054")


# .SHP file can't be read directly from the S3 bucket you . You would need to download/have the .SHP file in your local machine and then you have to convert the .SHP file to .GEOJSON/.PARQUET
# Code to convert to .geojson

#uk_border_temp <- sf::st_read("~/UKFSR2024/Transport Delay Shape File/CTRY_DEC_2022_GB_BUC.shp")
#sf::st_write(uk_border_temp, dsn = "CTRY_DEC_2022_GB_BUC.geojson", layer = 1)  # Assuming one layer

#shp_temp <- sf::st_read("~/UKFSR2024/Transport Delay Shape File/2022_Delay_v2.shp")
#sf::st_write(uk_border_temp, dsn = "2022_Delay_v2.geojson", layer = 1)

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
FSR_3_1_7_plot <- ggplot() +
              # Add UK border as the base layer
              geom_sf(data = uk_border, aes(fill = CTRY22NM), color = "black", size = 0.5) +
              scale_fill_manual(values = fill_colors) +
              guides(fill = FALSE) +
              
              # Add transport delay data with color encoding on top
              geom_sf(data = shp,
                      aes(color = factor(cut(AvgDelay, breaks = delay_breaks, labels = delay_labels, include.lowest = TRUE))),
                      linewidth = 1) +
              
              #labs(title = "2022 Transport Delay (seconds/vehicle/mile)") +
              
              scale_color_manual(values = c(af_colors),
                                 labels = delay_labels,
                                 name = "Avg Delay (seconds/vehicle/mile)",
                                 drop = FALSE) + 
              theme_ukfsr() +
              
              theme_void()

FSR_3_1_7_plot  

# Output the plot
save_graphic(FSR_3_1_7_plot, '3.1.7','Road Congestion and Travel Time Statistics (average speed and delay)') 
#+ save_csv(FSR_3_1_7, '3.1.7','Road Congestion and Travel Time Statistics (average speed and delay)')

------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Support 1 : Average delay per vehicle/second
  
FSR_3_1_7a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_3/input_data/3_1_3c_ave_delay_Strategic_Road_Network.csv")

FSR_3_1_7a <- FSR_3_1_7a %>%
  gather(variable,value, `Monthly`,`Year ending`)


FSR_3_1_7a$Month <- lubridate::dmy(paste("01", FSR_3_1_7a$Month))

print(FSR_3_1_7a)



# Create the plot
FSR_3_1_7a_plot <- ggplot(FSR_3_1_7a, aes(x = Month, y = value, colour = variable, group = variable, label = variable)) +
  theme_ukfsr() +
  
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

FSR_3_1_7a_plot

# Output the plot
save_graphic(FSR_3_1_7a_plot, '3.1.7a','Average Delay (second per vehicle)') + save_csv(FSR_3_1_7a, '3.1.7a','Average Delay (second per vehicle)')
