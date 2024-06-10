library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('scales')

source(here::here("utils", "load-font.R"))

contents <- get_bucket_df("s3-ranch-054")

#Average Distance Travelled by English Region, 2022

FSR_4_1_3a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/4.1.3 Average Distance Travelled by English Region.csv")

colnames(FSR_4_1_3a) <- c("Year", "Region", "Miles", "Regional_Code")

FSR_4_1_3a <- FSR_4_1_3a %>%
  arrange(desc(Regional_Code)) %>%
  mutate(Region = factor(Region, levels = Region))

FSR_4_1_3a_plot <- ggplot(FSR_4_1_3a, aes(x= Region, y=Miles)) +
  geom_bar(stat="identity", show.legend = FALSE, fill = af_colours(n=1)) +
  geom_text(aes(label = Miles), vjust= 0.3, hjust = 1.2, size=6, color='white', parse = FALSE) +  
  scale_y_continuous(limits = c(0,300), breaks=seq(0,300,50)) +
  theme_ukfsr()+
  coord_flip() +
  theme(axis.title.x=element_text(size=20)) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16)) +
    labs(x = "Region", 
         y = "Miles per person per year") + 
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.x = element_line( size=.1, color="black" ) 
  ) 


FSR_4_1_3a_plot


save_graphic(FSR_4_1_3a_plot, '4.1.3a','Average Distance Travelled by English Region, 2022') + 
  save_csv(FSR_4_1_3a, '4.1.3a','Average Distance Travelled by English Region, 2022')


------------------------------------------------------------------------------------------------------------------------------------------------
  

#  Sum of Supermarkets per 10,000 People by Region
  
FSR_4_1_3b <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/4.1.3 Access to supermarket ONS.csv")

colnames(FSR_4_1_3b) <- c("Region", "Supermarket_count","Region_Code")


FSR_4_1_3b <- FSR_4_1_3b %>%
  arrange(desc(Region_Code)) %>%
  mutate(Region = factor(Region, levels = Region))

FSR_4_1_3b_plot <- ggplot(FSR_4_1_3b, aes(x= Region, y=Supermarket_count)) +
  geom_bar(stat="identity", show.legend = FALSE, fill = af_colours(n=1)) +
  geom_text(aes(label = Supermarket_count), vjust= 0.3, hjust = 1.2, size=6, color='white', parse = FALSE) +  
  scale_y_continuous(limits = c(0,200), breaks=seq(0,200,25)) +
  theme_ukfsr()+
  coord_flip() +
  theme(axis.title.x=element_text(size=20)) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16)) +
  labs(x = "Region", 
       y = "Number of supermarkets per 10,000 people") + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.x = element_line( size=.1, color="black" ) 
  ) 


FSR_4_1_3b_plot


save_graphic(FSR_4_1_3b_plot, '4.1.3b','Sum of Supermarkets per 10,000 People by Region') + 
  save_csv(FSR_4_1_3b, '4.1.3b','Sum of Supermarkets per 10,000 People by Region')

