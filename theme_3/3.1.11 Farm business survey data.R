library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# FBS net margin data ----------------------------------------------------------
  net_margins <- aws.s3::s3read_using(FUN = read_csv,
                                       bucket = ukfsr::s3_bucket(),
                                       object = "theme_3/input_data/3_1_11_fbs_net_margins.csv")

  
 margin_cht <-  net_margins |> 
    filter(category != "Farm Business") |> 
    mutate(category = factor(category,
                             levels = rev(c("Agriculture",
                                            "Agri-environment", 
                                            "Diversification",
                                            "Direct Payments"))),
           farm_type = factor(farm_type,
                              levels = rev(c("All Types", 
                                         "Mixed",
                                         "Specialist Poultry",
                                         "Specialist Pigs", 
                                         "Grazing Livestock (Lowland)",
                                         "Grazing Livestock (Less Favoured Area)",
                                         "Dairy",
                                         "Horticulture",
                                         "General Cropping",
                                         "Cereals")),
                              labels = rev(c("All farms", 
                                         "Mixed",
                                         "Poultry",
                                         "Pigs", 
                                         "Grazing Livestock\n(Lowland)",
                                         "Grazing Livestock\n(Less Favoured Area)",
                                         "Dairy",
                                         "Horticulture",
                                         "General cropping",
                                         "Cereals")))) |> 
    ggplot() +
    geom_col(aes(x = farm_type, y = value, fill = category)) +
    scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
    scale_fill_manual(values = af_colours(n = 4)) +
    guides(fill = guide_legend(nrow=2, byrow = TRUE)) +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
    theme(axis.line.y = element_line(colour = "white"))

 
save_graphic(margin_cht, "3.1.11a", "fbs net margins")  
  
  
# FBS economic performance data ----------------------------------------------------------
econ <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_3/input_data/3_1_11_fbs_economic_performance.csv")  


econ_cht <- econ |> 
  filter(category != "top_25_percent_vs_bottom_25_percent") |> 
  mutate(farm_type = factor(farm_type,
                            levels = rev(c("All Types", 
                                           "Mixed",
                                           "Specialist Poultry",
                                           "Specialist Pigs", 
                                           "Grazing Livestock (Lowland)",
                                           "Grazing Livestock (Less Favoured Area)",
                                           "Dairy",
                                           "Horticulture",
                                           "General Cropping",
                                           "Cereals")),
                            labels = rev(c("All farms", 
                                           "Mixed",
                                           "Poultry",
                                           "Pigs", 
                                           "Lowland Grazing\nLivestock",
                                           "LFA Grazing\nLivestock",
                                           "Dairy",
                                           "Horticulture",
                                           "General cropping",
                                           "Cereals"))),
         category = factor(category, 
                           levels = c("bottom_25_percent",
                                      "middle_50_percent",
                                      "top_25_percent"),
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))) |> 
  ggplot() +
  geom_point(aes(x = farm_type, y = value, colour = category, shape = category), size = 5) +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_colour_manual(values = af_colours(type = "sequential")) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(panel.grid.major.y = element_line(colour = "#cbcbcb"))
  
                               
                                
save_graphic(econ_cht, "3.1.11b", "fbs economic performance")                                 
                       
  
           
                            
                                  
                         
                     
  