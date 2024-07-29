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

glu_simple <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/t1_1_8/output/csv/1_1_8_global_land_use_for_food_production_simplified.csv")


glu_simple <- glu_simple |> 
    mutate(region = factor(region, levels = rev(c("Agricultural land", "Habitable land", "Land surface", "Earths surface")),
                           labels = rev(c("Agricultural\n land", "Habitable\n land", "Land\n surface", "Earth's\n surface"))),
           category = factor(category, levels = c("Land", "Barren land", "Habitable Land",  
                                                   "Forests", "Agriculture",   
                                                  "Non-food crops", "Crops for food", "Livestock"),
                             labels = c("Land", "Barren land,\nGlaciers","Habitable\nLand",  
                                         "Forests,\nWater bodies,\nUrban", "Agriculture",   
                                        "Non-food crops", "Food crops", "Livestock")),
           label_outside = case_when(category %in% c("Barren land,\nGlaciers",
                                                     "Forests,\nWater bodies,\nUrban",
                                                     "Non-food crops") ~ "a", .default = "b"))



cht_simple <- ggplot(data = glu_simple, aes(x = region, y = area)) +
    geom_col(aes(fill = category), colour = "white") +
    geom_text(aes(label = category, colour = label_outside), position = position_stack(vjust = 0.5),family = "GDS Transport Website", size = 6) +
    scale_fill_manual(values = c(Land = "#12436D",
                               `Barren land,\nGlaciers` = "#BFBFBF",
                               `Habitable\nLand` = "#12436D",  
                               `Forests,\nWater bodies,\nUrban` =  "#BFBFBF",
                               Agriculture = "#12436D",   
                               `Non-food crops` =  "#BFBFBF",
                               `Food crops` = "#12436D", 
                               Livestock = "#12436D")) +
  scale_colour_manual(values = c(a = "#12436D", b = "#BFBFBF")) +
  labs(x = NULL, 
       y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") + 
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line.y = element_blank(), 
        panel.grid.major.y = element_blank())
  
  
  
 save_graphic(cht_simple, "1.1.8", "global land use for food production simplified") 
 