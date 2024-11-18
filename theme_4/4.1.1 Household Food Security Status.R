# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
#library(aws.ec2.metadata)

library(stringr)
library(data.table)
library(ggrepel)
library(scales)
library(zoo)
library(plyr)
library(grid)
library(ggpp)
library(ggrepel)
library(png)
library(viridis)
library(showtext)
library(svglite)



library(here)
library(sf)
library(janitor)


# All hh food security ---------------------------------------------------------

FSR_4_1_1 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/4_1_4a_ household_food_security_status_FYE_2023 .csv")



FSR_4_1_1 <- FSR_4_1_1 %>%
  #gather(variable,value, `High`,`Marginal`,`Low`,`Very low`)
  gather(variable,value, `Very low`,`Low`,`Marginal`,`High`)


check_sum<-FSR_4_1_1 %>% 
  #arrange(Year) %>% 
  select(-variable) %>%
  group_by(Year) %>%
  summarise(sum =sum(value)) #check sums add to 100


# Turn question column into a character vector
FSR_4_1_1$variable <- as.character(FSR_4_1_1$variable)
# Then turn it back into a factor with the levels in the correct order
FSR_4_1_1$variable <- factor(FSR_4_1_1$variable, levels=unique(FSR_4_1_1$variable))

# Reverse the selected color in the graph
reversed_palette <- rev(af_colours()[1:length(unique(FSR_4_1_1$variable))])


source(here::here("utils", "load-font.R"))

FSR_4_1_1_plot <- ggplot(FSR_4_1_1, aes(x = Year, y = value, fill = variable, label = round(value,0))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position = position_stack(vjust = 0.5), 
            colour= "white",
            family = "GDS Transport Website",
            size = 7) +
  scale_fill_manual(values=reversed_palette, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Percentage household food security", x = NULL)+
  coord_flip() +
  # guides(colour=guide_legend(override.aes=list(size=1),reverse = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website",
              horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(axis.title.x=element_text(vjust=-0.5))



FSR_4_1_1_plot


save_graphic(FSR_4_1_1_plot, '4.1.1a','Household food security status of all households in the UK') 
  save_csv(FSR_4_1_1, '4.1.1a','Household food security status of all households in the UK')


# NOT USED % insecurity map ----------------------------------------------------
# Support 1 - % of households food (Insecure) Map Code:
  
FSR_4_1_1a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/4_1_4a_household_food_security_region_FYE_2023.csv")%>% clean_names()

#max(FSR_4_1_4a$food_insecure) >- highest insecurity is 13
#min(FSR_4_1_4a$food_insecure) >- lowest food insecurity is 8

nuts1 <- aws.s3::s3read_using(FUN = sf::st_read,
                              bucket = "s3-ranch-054",
                              object = "assets/maps/ukfsr_uk_nuts1/ukfsr_uk_nuts1.geojson")%>% clean_names()


#map with three bands
mapdata <- nuts1 %>% 
  left_join(FSR_4_1_1a) %>% 
  #mutate(insecure_bands = case_when(food_insecure < 8 ~ "5-7",
  #food_insecure >=8 & food_insecure < 10 ~ "8-9",
  #food_insecure >= 10 ~ "10-11"),
  mutate(insecure_bands = case_when(food_insecure < 10 ~ "8-9",
                                    food_insecure >=10 & food_insecure < 12 ~ "10-11",
                                    food_insecure >= 12 ~ "12-13")) %>%
  mutate(insecure_bands = factor(insecure_bands, levels = c("8-9","10-11","12-13"), ordered = TRUE))
#legend ordering not working?

FSR_4_1_1a_plot <- ggplot(mapdata) +
  
  geom_sf(aes(fill = insecure_bands),  lwd = 0.1) +
  geom_sf_text(aes(label = stringr::str_wrap(region, 5)), size = 3.2, colour = "black") +
  #scale_colour_manual(values=af_colours(type =c("categorical")) +
  scale_fill_manual(values = af_colours(type =c("categorical")),
                    name = "% of \nhouseholds \nthat are food\ninsecure") +
  theme_ukfsr()+
  theme_void() + 
  theme(legend.position = "inside", legend.position.inside = c(0.8,0.75))

FSR_4_1_1a_plot

#mapdata

# save_graphic(FSR_4_1_1a_plot, '4.1.1a','% of households food (Insecure)')

#ggsave(here("data", "FSR_4_1_1a_pct_hh_insecure_map.svg"), width = 960, height = 640, units = "px", dpi = 72)
#ggsave(here("data", "FSR_4_1_1a_pct_hh_insecure_map.png"), width = 960, height = 640, units = "px", dpi = 72)


# hh food security by disability -----------------------------------------------
  # Support 2 - Households food security by disability.

FSR_4_1_1b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/Household food security by diability.csv")



FSR_4_1_1b <- FSR_4_1_1b %>%
  #gather(variable,value, `High`,`Marginal`,`Low`,`Very low`)
  gather(variable,value, `Very low`,`Low`,`Marginal`,`High`)


check_sum<-FSR_4_1_1b %>% 
  #arrange(Year) %>% 
  select(-variable) %>%
  group_by(Disability) %>%
  summarise(sum =sum(value)) #check sums add to 100


# Turn question column into a character vector
FSR_4_1_1b$variable <- as.character(FSR_4_1_1b$variable)
# Then turn it back into a factor with the levels in the correct order
FSR_4_1_1b$variable <- factor(FSR_4_1_1b$variable, levels=unique(FSR_4_1_1b$variable))

# Reverse the selected color in the graph
reversed_palette <- rev(af_colours()[1:length(unique(FSR_4_1_1b$variable))])


source(here::here("utils", "load-font.R"))

FSR_4_1_1b_plot <- ggplot(FSR_4_1_1b, aes(x = Disability, y = value, fill = variable, label = round(value,0))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position = position_stack(vjust = 0.5), 
            colour= "white",
            family = "GDS Transport Website",
            size = 7) +
  scale_x_discrete(labels = label_wrap_gen()) +
  scale_fill_manual(values=reversed_palette, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Percentage household food security", x = NULL)+
  coord_flip() +
  # guides(colour=guide_legend(override.aes=list(size=1),reverse = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website",
              horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(axis.title.x=element_text(vjust=-0.5))


FSR_4_1_1b_plot

save_graphic(FSR_4_1_1b_plot, '4.1.1c','Household food security status by disability') 
save_csv(FSR_4_1_1b, '4.1.1c','Household food security status by disability')

# hh food security x age -------------------------------------------------------
  
  # Support 3 - Households food security by age.
  
FSR_4_1_1c <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/Household food security by age normal.csv")



FSR_4_1_1c <- FSR_4_1_1c %>%
  #gather(variable,value, `High`,`Marginal`,`Low`,`Very low`)
  gather(variable,value, `Very low`,`Low`,`Marginal`,`High`)


check_sum<-FSR_4_1_1c %>% 
  #arrange(Year) %>% 
  select(-variable) %>%
  group_by(Age) %>%
  summarise(sum =sum(value)) #check sums add to 100


# Turn question column into a character vector
FSR_4_1_1c$variable <- as.character(FSR_4_1_1c$variable)
# Then turn it back into a factor with the levels in the correct order
FSR_4_1_1c$variable <- factor(FSR_4_1_1c$variable, levels=unique(FSR_4_1_1c$variable))

# Reverse the selected color in the graph
reversed_palette <- rev(af_colours()[1:length(unique(FSR_4_1_1c$variable))])





source(here::here("utils", "load-font.R"))

FSR_4_1_1c_plot <- ggplot(FSR_4_1_1c, aes(x = Age, y = value, fill = variable, label = round(value,0))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position = position_stack(vjust = 0.5), 
            colour= "white",
            family = "GDS Transport Website",
            size = 7) +
  scale_fill_manual(values=reversed_palette, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Percentage household food security", x = NULL)+
  coord_flip() +
  # guides(colour=guide_legend(override.aes=list(size=1),reverse = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website",
              horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(axis.title.x=element_text(vjust=-0.5))


FSR_4_1_1c_plot

save_graphic(FSR_4_1_1c_plot, '4.1.1d','Household food security status by age')
  save_csv(FSR_4_1_1c, '4.1.1d','Household food security status by age')


# hh food security x ethnicity -----------------------------------------------
  
  # Support 4 - Households food security by ethnicity.
  
FSR_4_1_1d <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/Household food security by ethinic group.csv")



FSR_4_1_1d <- FSR_4_1_1d %>%
  #gather(variable,value, `High`,`Marginal`,`Low`,`Very low`)
  gather(variable,value, `Very low`,`Low`,`Marginal`,`High`)


check_sum<-FSR_4_1_1d %>% 
  #arrange(Year) %>% 
  select(-variable) %>%
  group_by(`Ethnic group`) %>%
  summarise(sum =sum(value)) #check sums add to 100


# Turn question column into a character vector
FSR_4_1_1d$variable <- as.character(FSR_4_1_1d$variable)
# Then turn it back into a factor with the levels in the correct order
FSR_4_1_1d$variable <- factor(FSR_4_1_1d$variable, levels=unique(FSR_4_1_1d$variable))

# Reverse the selected color in the graph
reversed_palette <- rev(af_colours()[1:length(unique(FSR_4_1_1d$variable))])


source(here::here("utils", "load-font.R"))

FSR_4_1_1d_plot <- ggplot(FSR_4_1_1d, aes(x = `Ethnic group`, y = value, fill = variable, label = round(value,0))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(position = position_stack(vjust = 0.5), 
            colour= "white",
            family = "GDS Transport Website",
            size = 7) +
  scale_x_discrete(labels = label_wrap_gen()) +
  scale_fill_manual(values=reversed_palette, guide = guide_legend(reverse = TRUE)) +
  labs(y = "Percentage household food security", x = NULL)+
  coord_flip() +
  # guides(colour=guide_legend(override.aes=list(size=1),reverse = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website",
              horizontal = TRUE) +
  theme(legend.position = "bottom", legend.title = element_blank())+
  theme(axis.title.x=element_text(vjust=-0.5))


FSR_4_1_1d_plot

save_graphic(FSR_4_1_1d_plot, '4.1.1e','Household food security status by ethnicity')
save_csv(FSR_4_1_1d, '4.1.1e','Household food security status by ethnicity')
