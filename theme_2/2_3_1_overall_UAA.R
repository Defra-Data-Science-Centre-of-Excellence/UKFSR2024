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
                                object = "theme_2/t2_3_1/input/structure-june-uktimeseries-14dec23.ods",
                                sheet = "UK_timeseries", 
                                col_names = TRUE,
                                skip = 1)

current_year <- "X2023"
UAA_chart1 <- UAA_data%>%filter(Table.1..Land.use.a...hectares.=="Utilised agricultural area (UAA)")%>%
pivot_longer(cols = `X1984`:glue("{current_year}"),names_to = "year", values_to = "value") %>%
  mutate(year = substring(year,2,5))%>%
  mutate(year = as.numeric(year))%>%
  mutate(value = as.numeric(value))

#Chart code plots tfp_chart2 as a line plot
Overall_UAA_chart <- ggplot(UAA_chart1, aes(x = year, y = value/1e6)) +
  geom_line() +
          scale_x_continuous(expand = c(0,0), limits = c(2013,as.numeric(substr(current_year,2,5))+1), breaks = seq(2013, 2023, by = 2), labels = seq(2013, 2023, by = 2)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,19), breaks = seq(0, 19, by = 2), labels = seq(0, 19, by = 2)) +
   theme_ukfsr(base_family = "GDS Transport Website") +
  labs(y="Million Hectares") +
  theme(axis.text = element_text(family = "GDS Transport Website"),
        axis.line.x = element_line(colour =  "black"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 16, family = "GDS Transport Website"))

save_graphic(Overall_UAA_chart, "2.3.1", "overall UAA")