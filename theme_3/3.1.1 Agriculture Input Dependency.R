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

# Principal farm costs ---------------------------------------------------------

FSR_3_1_1 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/input_data/farm_costs.csv")

variable_order <- c("Fertilisers", "Pesticides", "Seeds", 
                    "Animal feed", "Veterinary expenses", "Energy", 
                    "Agricultural services", "Total maintenance", "Other goods and services")

variable_labels <- c("Fertilisers", "Pesticides", "Seeds", 
                    "Animal feed", "Veterinary expenses", "Energy", 
                    "Agricultural services", "Total maintenance", "Other goods & services")

FSR_3_1_1 <- FSR_3_1_1 %>%
  gather(variable,value, 'Seeds','Energy','Fertilisers','Pesticides','Animal feed',
         'Agricultural services','Veterinary expenses','Total maintenance','Other goods and services') %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01")),
         variable = factor(variable, levels = variable_order, labels = variable_labels))


FSR_3_1_1plot <- ggplot(FSR_3_1_1, aes(x=Year, y=value, group=variable, color = 'variable')) +
  geom_line() +
  facet_wrap( ~ variable) +
  scale_colour_manual(values = af_colours(),
                      labels = c("variable" = NULL)) + 
  #guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = NULL,
       y = "£ Million") +
  scale_x_date(breaks=seq(as.Date("2005-01-01"),Sys.Date()-lubridate::years(1),by = "5 year"),date_labels = "%Y") +
  theme_ukfsr(base_family = "GDS Transport Website",base_size = 12) +
  theme(plot.margin = margin(5,50,5,5,unit = "pt"),
        legend.position = "none")

FSR_3_1_1plot

save_graphic(FSR_3_1_1plot, '3.1.1a', 'UK principal farm costs final') 
save_csv(FSR_3_1_1, '3.1.1a', ' UK principal farm costs')

# Fertilizer Usage -------------------------------------------------------------
  
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

save_graphic(FSR_3_1_1aplot, '3.1.1b', 'UK Fertiliser usage final')  
save_csv(FSR_3_1_1a, '3.1.1b', ' UK Fertiliser usage')

# Nitrogen soil balance --------------------------------------------------------
# Data is table 5 from UK and England soil nutrients time series 
# https://www.gov.uk/government/statistics/uk-and-england-soil-nutrient-balances-2022

soil_balance <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/england_soil_balance_nitrogen.csv")


soil_balance <- soil_balance |> 
    mutate(nue = offtake/inputs)

nue_chart <- soil_balance |> ggplot() +
  geom_line(aes(x = year, y = nue), colour = af_colours(n=1)) +
  scale_y_continuous(limits = c(0,1), labels = scales::label_percent()) +
  labs(x= NULL, y = "percentage") +
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(nue_chart, "3.1.1c", "nitrogen use efficiency england final")
save_csv(soil_balance, "3.1.1c", "nitrogen use efficiency england")

# Pesticides Usage NOT USED-------------------------------------------------------------
  
# FSR_3_1_1b <- aws.s3::s3read_using(FUN = readr::read_csv,
#                                      bucket = "s3-ranch-054",
#                                      object = "theme_3/input_data/3_1_1_pesticide_usage.csv")


# Transform the data to long format for ggplot2
# FSR_3_1_1b <- FSR_3_1_1b %>%
#   gather(key = "measure", value = "value", kg, area) %>%
#   mutate(year = as.integer(year))

# FSR_3_1_1b <- FSR_3_1_1b %>%
#   mutate(value = case_when(
#     measure == "area" ~ value / 1e5,  # Convert hectares to millions of hectares
#     measure == "kg" ~ value / 1e5,    # Convert kilograms to thousands of tonnes
#     TRUE ~ value
#   ))

# Define breaks for the x-axis
# x_breaks <- seq(2002, max(FSR_3_1_1b$year), by = 4)

# Define a custom labeller to remove the measure name from the facets
# custom_labeller <- function(variable, value) {
#   if (variable == "measure") {
#     return(NULL)  # Return an empty string for measure
#   } else {
#     return(as.character(value))  # Return the value as is for other variables
#   }
# }

# Plot the line chart
# FSR_3_1_1bplot <- ggplot(FSR_3_1_1b, aes(x = year, y = value, colour = measure, group = measure)) +
#   geom_line() +
#   facet_wrap(~chemgroup + measure, labeller = custom_labeller) +
#   scale_color_manual(values = af_colours(),
#                      labels = c("kg" = "Weight (Thousand tonnes)", "area" = "Area (Million ha)")) +
#   scale_x_continuous(breaks = x_breaks, limits = c(2002, max(FSR_3_1_1b$year))) + 
#   labs(x = NULL,
#        y = NULL,
#        colour = "Chemical Group") +
#   theme_ukfsr() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.margin = margin(5,50,5,5,unit = "pt"))



# save_graphic(FSR_3_1_1bplot, '3.1.1b', ' UK Pesticides usage') + 
#   save_csv(FSR_3_1_1b, '3.1.1b', ' UK Pesticides usage')


# area <- ggplot(FSR_3_1_1b |> filter(measure == "area")) +
#   geom_line(aes(x = year, y = value), colour = af_colours(n =1)) +
#   facet_wrap(vars(chemgroup)) +
#   scale_x_continuous(breaks = x_breaks, limits = c(2002, max(FSR_3_1_1b$year))) +
#   labs(x = NULL, y = "Area (million ha)") +
#   theme_ukfsr(base_family = "GDS Transport Website", base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.margin = margin(5,50,5,5,unit = "pt"))

# save_graphic(area, '3.1.1c', 'Pesticides usage by area alternate')

# vol <- ggplot(FSR_3_1_1b |> filter(measure == "kg")) +
#   geom_line(aes(x = year, y = value), colour = af_colours(n =1)) +
#   facet_wrap(vars(chemgroup)) +
#   scale_x_continuous(breaks = x_breaks, limits = c(2002, max(FSR_3_1_1b$year))) +
#   labs(x = NULL, y = "Weight (thousand tonnes)") +
#   theme_ukfsr(base_family = "GDS Transport Website", base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.margin = margin(5,50,5,5,unit = "pt"))

# save_graphic(vol, '3.1.1d', 'Pesticides usage by weight alternate')

# Pesticides FERA data ---------------------------------------------------------

pesticides_fera <- aws.s3::s3read_using(FUN = readr::read_csv,
                             bucket = "s3-ranch-054",
                             object = "theme_3/input_data/pesticides_fera.csv")

fera_chart <- pesticides_fera |> 
  filter(name != "Total") |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = name)) +
  scale_y_continuous(labels = scales::label_number(scale = 0.001)) +
  scale_x_continuous(breaks = seq(2010, 2022, by = 2)) +
  scale_fill_manual(values = af_colours(n = 6)) +
  labs(x = NULL, y = "metric kilotons (kT)") +
  theme_ukfsr(base_family = "GDS Transport Website", x_axis = FALSE)

save_graphic(fera_chart, "3.1.1d", "pesticides fera final")
save_csv(pesticides_fera, "3.1.1d", "pesticides fera final")

# Animal feed ------------------------------------------------------------------

feed <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_3/input_data/animal_feed_use_auk_ch9.csv")

feed_chart <- ggplot(feed) +
  geom_line(aes(x = year, y = value, colour = Type)) +
  scale_colour_manual(values = af_colours(type = "categorical", n = 4)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = "thousand tonnes") +
  guides(colour = guide_legend(nrow = 2)) + 
  theme_ukfsr(base_family = "GDS Transport Website")

save_graphic(feed_chart, "3.1.1e", "animal feed use auk ch9 final")
save_csv(feed, "3.1.1e", "animal feed use")

# Collect charts for download --------------------------------------------------

charts <- ukfsr::bucket_manifest(file_ext = "svg") |> 
  dplyr::filter(stringr::str_starts(folder, "theme_3/t3_1_1/output"))

x <- charts$path
y <- charts$file

purrr::map2(x, y, \(x, y) {
  aws.s3::save_object(object = x, 
                      bucket = ukfsr::s3_bucket(), 
                      file = paste0("~/ukfsr/",y), 
                      headers = list("x-amz-acl" = "bucket-owner-full-control"))
}
)


zip(zipfile = "~/ukfsr/3_1_1.zip", files = list.files("~/ukfsr/", full.names = TRUE))
