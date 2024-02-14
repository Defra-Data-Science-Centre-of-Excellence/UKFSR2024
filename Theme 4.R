# install.packages("devtools")
devtools::install_github("FoodchainStats/ukfsr")

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
  ) +
  theme(text = element_text(family = "GDS Transport Website"))

FSR_4_1_1a_plot

save_graphic(FSR_4_1_1a_plot, '4.1.1a') + save_csv(FSR_4_1_1a, '4.1.1a') #save image and csv back to the bucket



