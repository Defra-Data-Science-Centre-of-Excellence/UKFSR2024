library(ggplot2)
library(dplyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

# https://ourworldindata.org/global-land-for-agriculture

glu <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_1/t1_1_8/output/csv/1_1_8_global_land_use_for_food_production.csv")

cht <- glu |> 
  mutate(region = factor(region, levels = c("Agricultural land", "Habitable land", "Land surface", "Earths surface"),
                         labels = c("Agricultural\n land", "Habitable\n land", "Land\n surface", "Earths\n surface")),
                         category = factor(category, levels = c("Land", "Barren land", "Glaciers","Habitable Land",  
                                                                "Waterbodies", "Urban", "Shrub", "Forests", "Agriculture",   
                                                                "Non-food crops", "Crops for food", "Livestock"))) |> 
  ggplot() +
  geom_col(aes(x = region, y = area, fill = category), colour = "white") +
  geom_text(aes(x = region, y = area, label = paste0(pct, "%\n", category, "\n", area, "M km ^ 2")),
            position = position_stack(vjust = 0.5),
            family = "GDS Transport Website", size = 6 ) +
  scale_fill_manual(values = c(Land = "#12436D",
                               `Barren land` = "#BFBFBF",
                               Glaciers = "#BFBFBF",
                               `Habitable Land` = "#12436D",  
                               Waterbodies =  "#BFBFBF",
                               Urban =  "#BFBFBF",
                               Shrub =  "#BFBFBF",
                               Forests =  "#BFBFBF",
                               Agriculture = "#12436D",   
                               `Non-food crops` =  "#BFBFBF",
                               `Crops for food` = "#12436D", 
                               Livestock = "#12436D")) +
  coord_flip() + 
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        panel.grid.major.y = element_blank())


save_graphic(cht, "1.1.8", "global land use for food production")

#  experiments------------------------------------------------------------------

glu2 <- glu |> 
  mutate(region = factor(region, levels = rev(c("Agricultural land", "Habitable land", "Land surface", "Earths surface")),
                         labels = rev(c("Agricultural\n land", "Habitable\n land", "Land\n surface", "Earth's\n surface"))),
         category = factor(category, levels = c("Land", "Barren land", "Glaciers","Habitable Land",  
                                                "Waterbodies", "Urban", "Shrub", "Forests", "Agriculture",   
                                                "Non-food crops", "Crops for food", "Livestock"),
                           labels = c("Land", "Barren land", "Glaciers","Habitable\nLand",  
                                      "Waterbodies", "Urban", "Shrub", "Forests", "Agriculture",   
                                      "Non-food crops", "Food crops", "Livestock")),
         label_outside = case_when(category %in% c("Barren land", "Glaciers", "Shrub", "Urban", "Waterbodies", "Forests", "Non-food crops") ~ "Y", .default = "N")) 


  ggplot() +
  geom_col(data = glu2, aes(x = region, y = area, fill = category), colour = "white") +
  geom_text(data = glu2 |> filter(label_outside == "N"), aes(x = region, y = area, label = paste0(pct, "% ", category)),
            position = position_stack(vjust = 0.5),
            family = "GDS Transport Website", size = 11, size.unit = "pt", colour = "#BFBFBF" ) +
    scale_fill_manual(values = c(Land = "#12436D",
                                 `Barren land` = "#BFBFBF",
                                 Glaciers = "#BFBFBF",
                                 `Habitable\nLand` = "#12436D",  
                                 Waterbodies =  "#BFBFBF",
                                 Urban =  "#BFBFBF",
                                 Shrub =  "#BFBFBF",
                                 Forests =  "#BFBFBF",
                                 Agriculture = "#12436D",   
                                 `Non-food crops` =  "#BFBFBF",
                                 `Food crops` = "#12436D", 
                                 Livestock = "#12436D")) +
    
  # coord_flip() + 
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") + 
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line.y = element_blank(), 
        panel.grid.major.y = element_blank())

