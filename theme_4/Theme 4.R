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


# GENERAL ADVICE FOR OUTPUTTING GRAPHICS AS svg (THE "REGULAR" png DIALOGUE IS COMMENTED OUT)
# Firstly, ensure you have the showtext and svglite packages installed. With the latter, if you are working on the SCE, it is probably worth checking 
# if the libcairo2-dev Ubuntu library is up to date (for the same reason if the outputs still aren’t quite right it might be worth installing 
# Cairo but I’m not sure if that causes a conflict with svglite so leave that for now). Then get in place the GDS Transport Website fonts. These are 
# the regular and bold typeface files. The main thing here is to create a folder entitled “font” in your working directory and place the font files 
# there. Then add: source("load-GDS-Transport.R") (putting the above .R file in the working directory) at the beginning of your script allows for the 
# loading of the fonts each time. To specify this font in ggplot(), the following should go at the bottom of your theme:
#   theme(text = element_text(family = "GDS Transport Website"))



contents <- get_bucket_df("s3-ranch-054")


#FSR_4_1_1a <- fread("4_1_1a_ave_share_spend_all_households_FYE_2020.csv")
FSR_4_1_1a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_4/input_data/4_1_1a_ave_share_spend_all_households_FYE_2020.csv")


# keep 9.0 when number is 8.99 (round will send 9)
# https://stackoverflow.com/questions/42105336/how-to-round-a-number-and-make-it-show-zeros/42105521
FSR_4_1_1a_plot <- ggplot(FSR_4_1_1a, aes(x=reorder(`Main household expenditure categories`, Percentage), y=Percentage, 
                                          fill=factor(ifelse(`Main household expenditure categories`=="Food & non-alcoholic drinks","Highlighted","Normal")))) +
  geom_bar(stat="identity", show.legend = FALSE) +
  geom_text(aes(label = sprintf('%.1f',Percentage)), vjust= 0.3, hjust = -0.4, size=6, parse = FALSE) +  
  scale_y_continuous(limits = c(0,15), breaks=seq(0,15,1)) +
  theme_ukfsr()+
  #scale_fill_manual(name = "area", values=c("#FDE725FF","#414487FF")) + #change this to afcolours?
  scale_colour_manual(values=af_colours(type =c("duo"),n=2)) +
  coord_flip() +
  theme(axis.title.x=element_text(size=20)) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16)) +
  labs(x = "Main household expenditure categories") +
  labs(y = "Percentage (%)") +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.x = element_line( size=.1, color="black" ) 
  ) 


FSR_4_1_1a_plot

save_graphic(FSR_4_1_1a_plot, '4.1.1a') + save_csv(FSR_4_1_1a, '4.1.1a') #save image and csv back to the bucket



#source(here("R", "load-GDS-Transport.R"))
#source(here("R", "palette_fsr.R"))


#frs <- read.csv(here("data", "frs_2019_2020_table9_1.csv")) %>% clean_names()
FSR_4_1_4a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/4_1_4a_household_food_security_region_FYE_2023.csv")%>% clean_names()

#max(FSR_4_1_4a$food_insecure) >- highest insecurity is 13
#min(FSR_4_1_4a$food_insecure) >- lowest food insecurity is 8

nuts1 <- st_read("~/R/UKFSR2024/UK map files/NUTS_Level_1_January_2018_FCB_in_the_United_Kingdom.shp")


#map with three bands
mapdata <- nuts1 %>% 
  left_join(FSR_4_1_4a) %>% 
  #mutate(insecure_bands = case_when(food_insecure < 8 ~ "5-7",
                                    #food_insecure >=8 & food_insecure < 10 ~ "8-9",
                                    #food_insecure >= 10 ~ "10-11"),
         mutate(insecure_bands = case_when(food_insecure < 10 ~ "8-9",
                                           food_insecure >=10 & food_insecure < 12 ~ "10-11",
                                           food_insecure >= 12 ~ "12-13"),
         insecure_bands = factor(insecure_bands, levels = c("8-9","10-11","12-13"), ordered = TRUE)) #legend ordering not working?

ggplot(mapdata) +
  
  geom_sf(aes(fill = insecure_bands),  lwd = 0.1) +
  geom_sf_text(aes(label = stringr::str_wrap(region, 5)), size = 2, colour = "black") +
  #scale_fill_fsr(palette = "map4",
  #               name = "% of households \nthat are food insecure") +
  theme_ukfsr()+
  theme_void()#+
  #scale_colour_manual(values=af_colours(type =c("categorical")))

#mapdata

ggsave(here("data", "FSR_4.1.4a_pct_hh_insecure_map.svg"), width = 960, height = 640, units = "px", dpi = 72)
ggsave(here("data", "FSR_4.1.4a_pct_hh_insecure_map.png"), width = 960, height = 640, units = "px", dpi = 72)



#FSR_4_2a <- fread("4_1_2a_ave_spend_food_non_alcohol_drinks_low_income_all_households.csv")
FSR_4_2a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/4_1_2a_ave_spend_food_non_alcohol_drinks_low_income_all_households_middle_income.csv")


F4_2a <- FSR_4_2a %>% 
  gather(key,value, `percentage spend on food and non-alcoholic drinks for all households`,`percentage spend on food and non-alcoholic drinks for middle 20% by income`, `percentage spend on food and non-alcoholic drinks for lowest 20% by income`)  %>%
  filter(Year>2011) %>% 
  mutate(key = case_when(key=="percentage spend on food and non-alcoholic drinks for all households"~"all households",
                         key=="percentage spend on food and non-alcoholic drinks for middle 20% by income"~"middle 20% by income",
                         key=="percentage spend on food and non-alcoholic drinks for lowest 20% by income"~"lowest 20% by income"))
         
 

F4_2a_plot <- ggplot(F4_2a, aes(x=factor(Year), y=value, colour=key, group=key)) +
  geom_line() +
  #  scale_y_continuous(labels=scales::percent, limits = c(0,0.2), breaks=seq(0,0.2,0.02)) +
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20,2)) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr()+
  scale_colour_manual(values=af_colours()) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)
  ) +
  theme(axis.title.x=element_blank()) +
  #  theme(axis.title.y=element_blank()) +
  labs(y = "% spend on food and non-alcoholic drinks") +
  geom_line(size=2) +
  #scale_colour_manual(name = "area", values=c("#FDE725FF","#414487FF")) +
  #  geom_point(size=NA) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=16)) +
  #  theme(axis.text.x = element_text(size=18, angle=45, vjust = 1, hjust=1)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(legend.text=element_text(size=22)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") +
  theme(text = element_text(family = "GDS Transport Website"))

F4_2a_plot


# ggsave(filename = "4_1_2a_ave_spend_food_non_alcohol_drinks_low_income_all_households.png", F4_2a_plot, scale = 1, device = "png", width = 32.6, height = 21.6, units = "cm", 
#        #       dpi = "screen")
#        dpi = 300)

ggsave(filename = "4_1_2a_ave_spend_food_non_alcohol_drinks.svg",
       F4_2a_plot,
       width = 960,
       height = 640,
       units = "px",
       dpi = 72)

save_graphic(F4_2a_plot, '4.2.2a') + save_csv(F4_2a, '4.2.2a') #save image and csv back to the bucket


FSR_4_1_4a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                   bucket = "s3-ranch-054",
                                   object = "theme_4/input_data/4_1_4a_ household_food_security_status_FYE_2023 .csv")



F4_5b <- FSR_4_1_4a %>%
  #gather(variable,value, `High`,`Marginal`,`Low`,`Very low`)
  gather(variable,value, `Very low`,`Low`,`Marginal`,`High`)


# https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# ensure question axis matches original vector
# https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
# Turn question column into a character vector
F4_5b$variable <- as.character(F4_5b$variable)
# Then turn it back into a factor with the levels in the correct order
F4_5b$variable <- factor(F4_5b$variable, levels=unique(F4_5b$variable))

# % stacked barplot with centred count numbers
# https://stackoverflow.com/questions/62323699/centre-and-offset-labels-in-ggplot2

# https://stackoverflow.com/questions/39156114/reversed-legend-using-guide-legend
# https://aosmith.rbind.io/2018/01/19/reversing-the-order-of-a-ggplot2-legend/


check_sum<-F4_5b %>% 
  #arrange(Year) %>% 
  select(-variable) %>%
  group_by(Year) %>%
  summarise(sum =sum(value))
  


FSR_4_1_4b_plot <- ggplot(F4_5b, aes(x = Year, y = value, fill = variable, label = value)) +
  geom_bar(stat = "identity", width = 0.5) +
  #geom_text(size = 8, position = position_stack(vjust = 0.5), colour= "white", fontface = "bold") +
  #theme_fsr()+
  #scale_fill_fsr(guide = guide_legend(reverse = TRUE)) +
  #scale_fill_manual(values=c("#FDE725FF", "#2A788EFF","#7AD151FF","#414487FF"), guide = guide_legend(reverse = TRUE)) +
  #  scale_fill_fsr(guide = guide_legend(reverse = TRUE)) +
  labs(y = "Percentage household food security") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  # geom_text(aes(label = value), size = 8, position=position_fill(vjust = 1.5), colour= "white", fontface = "bold") +
  coord_flip() +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_text(size=20)) +
  #  theme(axis.text.x = element_text(size=18, angle=45, vjust = 1, hjust=1)) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(legend.text=element_text(size=22))  +
  theme(text = element_text(family = "GDS Transport Website"))+
  theme_ukfsr(horizontal = TRUE)+
  scale_fill_manual(values = af_colours())+
  guides(colour=guide_legend(override.aes=list(size=1),reverse = TRUE)) + #reverse equal to true did not fix
  #guides(colour=guide_legend(override.aes=list(size=1),reverse = FALSE)) +
  #scale_colour_manual(values=af_colours(type =c("categorical"),n=4))+ 
  theme(panel.background = element_blank()) #+
  #scale_fill_fsr(guide = guide_legend(reverse = TRUE))

# guide_legend(reverse = TRUE)

FSR_4_1_4b_plot

