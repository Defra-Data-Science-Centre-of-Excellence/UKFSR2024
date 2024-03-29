library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')

contents <- get_bucket_df("s3-ranch-054")

FSR_3_1 <- aws.s3::s3read_using(FUN = readr::read_csv,
                          bucket = "s3-ranch-054",
                          object = "theme_3/input_data/3_1_2a_aggregate_energy_demand_agric_food_drink_manufact.csv")

F3_1a <- FSR_3_1 %>%
  gather(key,value, `Agriculture`, `Food and drink manufacturing`) # %>%


F3_1a_plot <- ggplot(F3_1a, aes(x=factor(Year), y=value, colour=key, group=key)) +
  #  geom_line() +
  scale_y_continuous(limits = c(0,4500), breaks=seq(0,4500,500)) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr()+
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)) +
  theme(axis.title.x=element_blank()) +
  # theme(axis.title.y=element_blank())
  labs(y = "Thousand tonnes oil equivalent") +
  geom_line(size=1.5) +
  scale_colour_manual(values=c("#FDE725FF","#414487FF")) +
  #  geom_point(size=NA) +
  theme(axis.title.y=element_text(size=20)) +
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 1, hjust=1)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(legend.text=element_text(size=22)) +
  guides(colour=guide_legend(override.aes=list(size=1))) 

F3_1a_plot

save_graphic(F3_1a_plot, '3.1.2a') + save_csv(F3_1a, '3.1.2a')

