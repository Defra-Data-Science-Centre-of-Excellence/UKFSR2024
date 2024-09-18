# install.packages("devtools")
#devtools::install_github("FoodchainStats/ukfsr")

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

FSR_4_1_6 <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_4/input_data/4.1.3 Average Distance Travelled by English Region.csv")

colnames(FSR_4_1_6) <- c("Year", "Region", "Miles", "Regional_Code", "Average_Distance")

FSR_4_1_6 <- FSR_4_1_6 %>%
  arrange(desc(Regional_Code)) %>%
  mutate(Region = factor(Region, levels = Region))

FSR_4_1_6_plot <- ggplot(FSR_4_1_6, aes(x= Region, y=Average_Distance)) +
  geom_bar(stat="identity", show.legend = FALSE, fill = af_colours(n=1)) +
  geom_text(aes(label = round(Average_Distance,1)), vjust= 0.3, hjust = -0.3, size=7, color='black', parse = FALSE) +  
  scale_y_continuous(limits = c(0,5), breaks=seq(0,5,1)) +
  theme_ukfsr()+
  coord_flip() +
  theme(axis.title.x=element_text(size=20)) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16)) +
    labs(x = "Region", 
         y = "Average distance travelled per person per trip in miles") + 
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.x = element_line( size=.1, color="black" ) 
  ) 


FSR_4_1_6_plot


save_graphic(FSR_4_1_6_plot, '4.1.6','Average Distance Travelled by English Region, 2022') + 
  save_csv(FSR_4_1_6, '4.1.6','Average Distance Travelled by English Region, 2022')


------------------------------------------------------------------------------------------------------------------------------------------------
  

 ## Support 1 : Sum of Supermarkets per 10,000 People by Region
  
FSR_4_1_6a <- aws.s3::s3read_using(FUN = readr::read_csv,
                                     bucket = "s3-ranch-054",
                                     object = "theme_4/input_data/4.1.3 Access to supermarket ONS.csv")

colnames(FSR_4_1_6a) <- c("Region", "Supermarket_count","Region_Code")


FSR_4_1_6a <- FSR_4_1_6a %>%
  arrange(desc(Region_Code)) %>%
  mutate(Region = factor(Region, levels = Region))

FSR_4_1_6a_plot <- ggplot(FSR_4_1_6a, aes(x= Region, y=Supermarket_count)) +
  geom_bar(stat="identity", show.legend = FALSE, fill = af_colours(n=1)) +
  geom_text(aes(label = Supermarket_count), vjust= 0.3, hjust = -0.3, size=7, color='black', parse = FALSE) +  
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


FSR_4_1_6a_plot


save_graphic(FSR_4_1_6a_plot, '4.1.6a','Sum of Supermarkets per 10,000 People by Region') + 
  save_csv(FSR_4_1_6a, '4.1.6a','Sum of Supermarkets per 10,000 People by Region')

