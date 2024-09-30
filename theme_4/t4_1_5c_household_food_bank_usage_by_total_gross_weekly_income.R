library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(data.table)

source(here("utils", "load-font.R"))

t4_1_5c <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_4/t4_1_2/output/csv/4_1_5c_household_food_bank_usage_by_total_gross_weekly_income.csv")

# https://stackoverflow.com/questions/76540961/special-characters-when-reading-in-csv-file-in-r
# t4_1_5c <- fread("4_1_5c_household_food_bank_usage_by_total_gross_weekly_income.csv", encoding = 'Latin-1')

t4_1_5c$`Age group` <- factor(t4_1_5c$`Age group`, levels = c("Less than £200","From £200 but less than £400","From £400 but less than £600",
                                                              "From £600 but less than £800","From £800 but less than £1,000","£1,000 or more"))

t4_1_5c_long <- t4_1_5c |> 
  group_by(`Age group`) |>
  pivot_longer(cols=c("30 day food bank usage","12 month food bank usage"),
               names_to="Usage",
               values_to="Value")

t4_1_5c_long$Usage <- factor(t4_1_5c_long$Usage, levels = c("30 day food bank usage","12 month food bank usage"))

level_order <- c("30 day food bank usage","12 month food bank usage")

af_categorical_colours <- afcolours::af_colours("duo")
names(af_categorical_colours)=levels(t4_1_5c_long$Usage)

t4_1_5c_long$Value[t4_1_5c_long$Value == 0] <- NA

t4_1_5c_long$age_wrap = str_wrap(t4_1_5c_long$`Age group`, width = 12)

# https://kohske.wordpress.com/2010/12/25/various-position-adjustments-of-legend-in-ggplot2/
t4_1_5c_plot <- ggplot(t4_1_5c_long, aes(x=factor(age_wrap), y=Value, fill=Usage)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits = c(0,9), breaks=seq(0,9,1)) +
  geom_text(data = subset(t4_1_5c_long, Usage == "30 day food bank usage"),
            aes(label=Value, vjust = -0.5, hjust = 2.7), size=8) +
  geom_text(data = subset(t4_1_5c_long, Usage == "12 month food bank usage"),
            aes(label=Value, vjust = -0.5, hjust = -1.8), size=8) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  theme(
    legend.position = "bottom", 
    legend.justification = c(0,0)
  ) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size=20)) +
  theme(legend.text=element_text(size=24)) +
  labs(y = "Percentage of households (%)") +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  theme(legend.direction = "vertical", legend.position = "bottom", legend.box = "vertical") 

t4_1_5c_plot

save_graphic(t4_1_5c_plot, "4.1.5c", "household food bank usage by total gross weekly income")

save_csv(t4_1_5c, "4.1.5c", "household food bank usage by total gross weekly income")
