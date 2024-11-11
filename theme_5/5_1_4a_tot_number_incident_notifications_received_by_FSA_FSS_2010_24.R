library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)

source(here("utils", "load-font.R"))

t5_1_4a <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t_5_1_4/output/csv/5_1_4a_tot_number_incident_notifications_received_by_FSA_FSS_2010_24.csv",
                                col_types = colspec)

t5_1_4a_long <- t5_1_4a %>%
  gather(variable,value,`Total incidents received by FSA (inclusive of FSS from 2015 onwards)`,`Total incidents received by FSS`,`Total Incidents received by FSA & FSS (FSS new data reporting format)`) 

af_categorical_colours <- afcolours::af_colours("categorical", n = 3)
names(af_categorical_colours)=levels(t5_1_4a_long$Response)

# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/
t5_1_4a_plot <- ggplot(t5_1_4a_long, aes(x=factor(Year), y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(data = subset(t5_1_4a_long, variable == "Total incidents received by FSA (inclusive of FSS from 2015 onwards)"), 
            aes(label=value ,vjust = -0.2, hjust = 0.9), size=7) +
  geom_text(data = subset(t5_1_4a_long, variable == "Total incidents received by FSS"), 
            aes(label=value, vjust = -0.2, hjust = -0.1), size=7) +
  geom_text(data = subset(t5_1_4a_long, variable == "Total Incidents received by FSA & FSS (FSS new data reporting format)"), 
            aes(label=value, vjust = -0.2, hjust = 0.5), size=7) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)
  ) +
  theme(axis.title.x=element_blank()) +
  theme(legend.text=element_text(size=24)) +
  labs(x = "Year") +
  labs(y = "Number of incidents") +
  theme(axis.text.x = element_text(size=24, angle=45, vjust = 1, hjust=1)) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") 

t5_1_4a_plot

save_graphic(t5_1_4a_plot, "5.1.4a", "tot number incident notifications received by FSA FSS 2010 24")

save_csv(t5_1_4a, "5.1.4a", "tot number incident notifications received by FSA FSS 2010 24")
