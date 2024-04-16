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
       ukfsr,
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
       aws.ec2metadata
)

current_year <- 2022

#Load Data
#Data is wide, where the first column is the account item and all the other columns are years (1973-2022)

tfp_data <- aws.s3::s3read_using(FUN = readr::read_csv,
                                bucket = "s3-ranch-054",
                                object = "theme_2/t2_2_3/input/tfp-data.csv",
                                col_names = TRUE,) %>%

#tfp_data <- aws.s3::s3read_using(FUN = read_csv(),
#                                 bucket = ukfsr::s3_bucket(),
#                                 object = "theme_2/t2_2_3/input/tfp-data.csv",
#                                 sheet = "tfp-data", 
#                                 col_names = TRUE,) %>%
  tibble() %>%
  mutate(account_item = str_replace_all(account_item,"cattle_for_meat", "beef")) %>%
  mutate(account_item = str_replace_all(account_item,"sheep_for_meat", "mutton_and_lamb")) %>%
  mutate(account_item = str_replace_all(account_item,"pigs_for_meat", "pigmeat")) %>%
  mutate(account_item = str_replace_all(account_item,"poultry_for_meat", "poultry"))

#The data is then pivoted longer and then assigns each account item to a general category
#The resulting dataframe "tfp_chart2" is a long df with three columns:
#account_item - either "all outputs", "all inputs" or "total factor productivity"
#year - item year
#value - value of that item in that year

tfp_chart2 <- tfp_data %>% 
  filter(account_item %in% c("all_outputs", "all_inputs", "total_factor_productivity")) %>%
  pivot_longer(cols = `1973`:glue("{current_year}"),names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(account_item = str_replace_all(account_item, "_", " ")) %>% 
  mutate(account_item = str_to_sentence(account_item))

#This code then finds three values: the current year value for each of the generalcategory:
#"all outputs", "all inputs" and "total factor productivity"

tfp_y <- tfp_chart2 %>%
  filter(account_item =="Total factor productivity") %>%
  filter(year == current_year) %>%
  pull(value)

outputs_y <- tfp_chart2 %>%
  filter(account_item =="All outputs") %>%
  filter(year == current_year) %>%
  pull(value)

inputs_y <- tfp_chart2 %>%
  filter(account_item =="All inputs") %>%
  filter(year == current_year) %>%
  pull(value)


#Chart code plots tfp_chart2 as a line plot
fig2 <- ggplot(tfp_chart2, aes(x = year, y = value, colour = account_item)) +
  geom_line(size = 3) +
  #geom_point(size= 4, shape = 21 , stroke = 2, fill = "white") +
  annotate(geom = "label",
           x = c(current_year-3, current_year-3, current_year-3), y = c(tfp_y + 12, outputs_y -16, inputs_y-12),
           label = c("Total factor productivity", "All outputs", "All inputs"),
           colour = c("#12436d","#28a197","#801650"),
           size = 7, family = "GDS Transport Website", fontface = "bold", fill = "white", label.size = NA) +
  scale_x_continuous(expand = c(0,0), limits = c(2012,current_year+1), breaks = seq(2012, 2022, by = 2), labels = seq(2012, 2022, by = 2)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,190), breaks = seq(0, 175, by = 25), labels = seq(0, 175, by = 25)) +
  scale_colour_manual(limits = c("Total factor productivity", "All outputs", "All inputs"), values = af_colours("categorical"))+
  #labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  theme(axis.text = element_text(family = "GDS Transport Website"),
        axis.line.x = element_line(colour =  "black"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 16, family = "GDS Transport Website"),
        legend.position = "none")

save_graphic(fig2, "2.2.3", "total factor productivity")