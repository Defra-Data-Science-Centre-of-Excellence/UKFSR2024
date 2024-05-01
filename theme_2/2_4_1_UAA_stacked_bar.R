#Packages

if(!require(pacman)) {
  install.packages("pacman")

  library(pacman)
}
# load other packages
p_load(cli,
       english,
       forcats,
       fs,
       ggplot2,
       gghighlight,
       gtable,
       grid,
       glue,
       here,
       knitr,
       lemon,
       lubridate,
       openxlsx,
       pander,
       plyr,
       readODS,
       scales,
       svglite,
       tidyverse,
       dplyr,
       unikn,
       showtext,
       aws.ec2metadata,
       ukfsr
)

#current_year <- 2022

#Load Data
#Data is wide, where the first column is the account item and all the other columns are years (1973-2022)

UAA_data <- aws.s3::s3read_using(FUN = read_ods,
                                bucket = "s3-ranch-054",
                                object = "theme_2/t2_4_1/input/structure-june-uktimeseries-14dec23.ods",
                                sheet = "UK_timeseries", 
                                col_names = TRUE,
                                skip = 1)

current_year <- "X2023"
UAA_chart1 <- UAA_data%>%filter(Table.1..Land.use.a...hectares.%in% c("Utilised agricultural area (UAA)", "Common rough grazing", "Total crops", "Uncropped arable land(d)", "Temporary grass under 5 years old", "Total permanent grassland"))%>%
pivot_longer(cols = `X1984`:glue("{current_year}"),names_to = "year", values_to = "value") %>%
  mutate(year = substring(year,2,5))%>%
  mutate(year = as.numeric(year))%>%
  mutate(value = as.numeric(value))%>%
  filter(year>2013)
  
UAA_plot <- ggplot(UAA_chart1, aes(x = year, y = value/1e6, fill = `Table.1..Land.use.a...hectares.`, label = round(value,2))) +
  geom_bar(position = "stack",stat = "identity", width = 0.5) +
  theme_ukfsr()+
  scale_fill_manual(values=af_colours(), guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks=seq(2014,2023,1)) +
  labs(y = "Million Hectares")+
  geom_text(aes(label = round(value/1e6,1)), size = 6, position=position_stack(vjust = 0.5), colour= "white", fontface = "bold") +
  theme(text = element_text(family = "GDS Transport Website"), legend.key.spacing.x = unit(0.1, 'cm'))

save_graphic(UAA_plot, "2.4.1", "UAA_by_type")