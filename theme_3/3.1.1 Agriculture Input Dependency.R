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

FSR_3_1_1 <- FSR_3_1_1 %>%
  gather(variable,value, 'Seeds','Energy','Fertilisers','Pesticides','Animal feed', 'Agricultural services') %>%
  mutate("Year" = as.Date(paste0(Year, "-01-01")))


FSR_3_1_1plot <- ggplot(FSR_3_1_1, aes(x=Year, y=value, colour=variable, group=variable)) +
  geom_line() +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(x = NULL,
       y = "£ Million") +
  scale_x_date(breaks=seq(as.Date("2003-01-01"),Sys.Date()-lubridate::years(1),by = "2 year"),date_labels = "%Y") +
  theme_ukfsr(base_family = "GDS Transport Website") 

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