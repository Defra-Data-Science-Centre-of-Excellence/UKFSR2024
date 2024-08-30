#devtools::install_github("FoodchainStats/ukfsr")


library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')
library('zoo')

source(here::here("utils", "load-font.R"))

FSR_3_1_1 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/2.2.1aFarmCosts.csv")

variable_order <- c("Fertilisers", "Pesticides", "Seeds", 
                    "Animal feed", "Veterinary expenses", "Energy", 
                    "Agricultural services", "Total maintenance", "Other goods and services")


FSR_3_1_1 <- FSR_3_1_1 %>%
  gather(variable,value, 'Seeds','Energy','Fertilisers','Pesticides','Animal feed',
         'Agricultural services','Veterinary expenses','Total maintenance','Other goods and services') %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01")),
         variable = factor(variable, levels = variable_order))


FSR_3_1_1plot <- ggplot(FSR_3_1_1, aes(x=Year, y=value, group=variable, color = 'variable')) +
  geom_line() +
  facet_wrap( ~ variable) +
  scale_colour_manual(values = af_colours(),
                      labels = c("variable" = NULL)) + 
  #guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = NULL,
       y = "£ Million") +
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "4 year"),date_labels = "%Y") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(plot.margin = margin(5,50,5,5,unit = "pt"),
        legend.position = "none")

FSR_3_1_1plot

save_graphic(FSR_3_1_1plot, '3.1.1', ' UK principal farm costs') + 
  save_csv(FSR_3_1_1, '3.1.1', ' UK principal farm costs')

----------------------------------------------------------------------------------------------------------------------------------------------
  ### Support 1 : Fertilizer Usage Graph
  
FSR_3_1_1a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                    bucket = "s3-ranch-054",
                                    object = "theme_3/input_data/Fertiliserusage.csv")

FSR_3_1_1a <- FSR_3_1_1a %>%
  gather(variable,value, 'Nitrogen (kt)','Phosphate (kt)','Potash (kt)') %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01")))


FSR_3_1_1aplot <- ggplot(FSR_3_1_1a, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = NULL,
       y = "Thousand tonnes") +
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),date_labels = "%Y") +
  #scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(plot.margin = margin(5,50,5,5,unit = "pt"))

FSR_3_1_1aplot

save_graphic(FSR_3_1_1aplot, '3.1.1a', ' UK Fertiliser usage') + 
  save_csv(FSR_3_1_1a, '3.1.1a', ' UK Fertiliser usage')

------------------------------------------------------------------------------------------------------------------------------------------------
  ### Support 2 : Pesticides Usage Graph
  
FSR_3_1_1b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_3/input_data/3_1_1_pesticide_usage.csv")


# Transform the data to long format for ggplot2
FSR_3_1_1b <- FSR_3_1_1b %>%
  gather(key = "measure", value = "value", kg, area) %>%
  mutate(year = as.integer(year))

FSR_3_1_1b <- FSR_3_1_1b %>%
  mutate(value = case_when(
    measure == "area" ~ value / 1e5,  # Convert hectares to millions of hectares
    measure == "kg" ~ value / 1e5,    # Convert kilograms to thousands of tonnes
    TRUE ~ value
  ))

# Define breaks for the x-axis
x_breaks <- seq(2002, max(FSR_3_1_1b$year), by = 4)

# Define a custom labeller to remove the measure name from the facets
custom_labeller <- function(variable, value) {
  if (variable == "measure") {
    return(NULL)  # Return an empty string for measure
  } else {
    return(as.character(value))  # Return the value as is for other variables
  }
}

# Plot the line chart
FSR_3_1_1bplot <- ggplot(FSR_3_1_1b, aes(x = year, y = value, colour = measure, group = measure)) +
  geom_line() +
  facet_wrap(~chemgroup + measure, labeller = custom_labeller) +
  scale_color_manual(values = af_colours(),
                     labels = c("kg" = "Weight (Thousand tonnes)", "area" = "Area (Million ha)")) +
  #scale_y_continuous(labels = label_number(big.mark = ",")) + 
  scale_x_continuous(breaks = x_breaks, limits = c(2002, max(FSR_3_1_1b$year))) + 
  labs(x = NULL,
       y = NULL,
       colour = "Chemical Group") +
  theme_ukfsr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5,50,5,5,unit = "pt"))


# Print the plot
print(FSR_3_1_1bplot)


save_graphic(FSR_3_1_1bplot, '3.1.1b', ' UK Pesticides usage') + 
  save_csv(FSR_3_1_1b, '3.1.1b', ' UK Pesticides usage')
