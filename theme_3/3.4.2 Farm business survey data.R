library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)

source(here::here("utils", "load-font.R"))

# FBS net margin data ----------------------------------------------------------
  net_margins <- aws.s3::s3read_using(FUN = read_csv,
                                       bucket = ukfsr::s3_bucket(),
                                       object = "theme_3/input_data/3_4_2_fbs_net_margins.csv")

  
 margin_cht <-  net_margins |> 
    filter(category != "Farm Business") |> 
    mutate(category = factor(category,
                             levels = rev(c("Agriculture",
                                            "Agri-environment", 
                                            "Diversification",
                                            "Direct Payments"))),
           farm_type = factor(farm_type,
                              levels = rev(c("All Types", 
                                         "Mixed",
                                         "Specialist Poultry",
                                         "Specialist Pigs", 
                                         "Grazing Livestock (Lowland)",
                                         "Grazing Livestock (Less Favoured Area)",
                                         "Dairy",
                                         "Horticulture",
                                         "General Cropping",
                                         "Cereals")),
                              labels = rev(c("All farms", 
                                         "Mixed",
                                         "Poultry",
                                         "Pigs", 
                                         "Grazing Livestock\n(Lowland)",
                                         "Grazing Livestock\n(Less Favoured Area)",
                                         "Dairy",
                                         "Horticulture",
                                         "General cropping",
                                         "Cereals")))) |> 
    ggplot() +
    geom_col(aes(x = farm_type, y = value, fill = category)) +
    scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
    scale_fill_manual(values = af_colours(n = 4)) +
    guides(fill = guide_legend(nrow=2, byrow = TRUE)) +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
    theme(axis.line.y = element_line(colour = "white"))

 
# save_graphic(margin_cht, "3.4.2f", "fbs net margins")  


# FBS mean incomes -------------------------------------------------------------

income <- aws.s3::s3read_using(FUN = read_csv,
                             bucket = ukfsr::s3_bucket(),
                             object = "theme_3/input_data/3_4_2_fbs_mean_income_2022_23.csv")  


income_data <- income |> 
  filter(survey_year %in% c("2021/22", "2022/23"),
         prices == "Current",
         variable == "Farm Business Income") |> 
  mutate(farm_type = factor(farm_type, levels = c("Cereals",
                                                  "General Cropping",
                                                  "Dairy", 
                                                  "Grazing Livestock (Lowland)", 
                                                  "Grazing Livestock (Less Favoured Area)",
                                                  "Specialist Pigs",
                                                  "Specialist Poultry", 
                                                  "Mixed",
                                                  "Horticulture",
                                                  "All Types"),
                            labels = c("Cereals",
                                       "General\nCropping",
                                       "Dairy", 
                                       "Grazing\nLivestock\n(Lowland)", 
                                       "Grazing\nLivestock\n(LFA)",
                                       "Specialist\nPigs",
                                       "Specialist\nPoultry", 
                                       "Mixed",
                                       "Horticulture",
                                       "All Types")))
income_cht <- income_data|> 
  ggplot(aes(x = farm_type, y = value, fill = survey_year)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(y = value, ymin = lower_ci, ymax = upper_ci),linewidth = 2, colour = "white", position = position_dodge(width = 0.9), width = 0.4) +
  geom_errorbar(aes(y = value, ymin = lower_ci, ymax = upper_ci),position = position_dodge(width = 0.9), width = 0.25) +
  scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
  scale_fill_manual(values = af_colours(type = "duo")) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website",base_size = 10,x_axis = FALSE)

save_csv(income_data, "3.4.2e", "fbs average farm business income")
save_graphic(income_cht, "3.4.2e", "fbs average farm business income")  

# FBS income by cost centre ----------------------------------------------------

# data collated from http://fbs.int.sce.network/shiny/FBS-GOVUK-dashboard/
# using real terms

fbs_data <- aws.s3::s3read_using(FUN = read_csv,
                                 bucket = ukfsr::s3_bucket(),
                                 object = "theme_3/input_data/fbs_all_data.csv")

cropping_types <- c("Cereals", "General cropping", "Mixed", "Horticulture")
livestock_types <- c("Dairy", "Lowland Grazing Livestock",
                     "LFA Grazing Livestock",
                     "Pigs", "Poultry")


cht_data <- fbs_data |> 
  # filter(stringr::str_detect(sda_status, "All"), stringr::str_detect(measure, "Farm Business Income")) |>
  mutate(chart_type = if_else(variable_label == "Farm Business Income", "point", "bar"),
         farm_category = case_when(level %in% cropping_types ~ "crops",
                                   level %in% livestock_types ~ "livestock",
                                   TRUE ~ "all"),
         variable_label = factor(variable_label, levels = c("Farm Business Income", "Direct Payment income", "Agricultural income", "Agri-environment income", "Diversified income"),
                                 labels = c("Farm Business Income", "Basic Payment Scheme", "Agriculture", "Agri-environment and other payments", "Diversification out of agriculture"))) |> 
  filter(survey_year %in% c("2022/23", "2020/21")) |> 
  select(survey_year, chart_type, farm_category, farm_type = level, cost_centre = variable_label, value)


livestock_chart <-  
  ggplot() +
  geom_col(data = cht_data |> filter(chart_type == "bar", farm_category %in% c("livestock", "all")),
           aes(x = survey_year, y = value, fill = cost_centre)) +
  geom_point(data = cht_data |> filter(chart_type == "point", farm_category %in% c("livestock", "all")),
             aes(x = survey_year, y = value, colour = cost_centre), size = 3, shape = 21, fill = "black", stroke = 2, inherit.aes = FALSE) +
  scale_colour_manual(values = "white") +
  scale_fill_manual(values = af_colours()) +
  scale_x_discrete(limits = rev(unique(cht_data$survey_year))) +
  scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
  facet_wrap(vars(farm_type), labeller = label_wrap_gen(width = 15),
             ncol = 1, dir = "h", strip.position = "left") +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(size = 20, hjust = 1, angle = 0),
        strip.background = element_rect(fill = "white"), legend.location = "plot") 

save_graphic(livestock_chart, "3.4.2f", "fbi livestock by cost centre")

crops_chart <-  
  ggplot() +
  geom_col(data = cht_data |> filter(chart_type == "bar", farm_category %in% c("crops", "all")),
           aes(x = survey_year, y = value, fill = cost_centre)) +
  geom_point(data = cht_data |> filter(chart_type == "point", farm_category %in% c("crops", "all")),
             aes(x = survey_year, y = value, colour = cost_centre), size = 3, shape = 21, fill = "black", stroke = 2, inherit.aes = FALSE) +
  scale_colour_manual(values = "white") +
  scale_fill_manual(values = af_colours()) +
  scale_x_discrete(limits = rev(unique(cht_data$survey_year))) +
  scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
  facet_wrap(vars(farm_type), labeller = label_wrap_gen(width = 15),
             ncol = 1, dir = "h", strip.position = "left") +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(size = 20, hjust = 1, angle = 0),
        strip.background = element_rect(fill = "white"), legend.location = "plot") 

save_graphic(crops_chart, "3.4.2g", "fbi cereals by cost centre")
save_csv(cht_data, "3.4.2", "farm business income by cost centre")

  
# NOT USED FBS economic performance data ---------------------------------------
econ <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_3/input_data/3_4_2_fbs_economic_performance.csv")  


econ_cht <- econ |> 
  filter(category != "top_25_percent_vs_bottom_25_percent") |> 
  mutate(farm_type = factor(farm_type,
                            levels = rev(c("All Types", 
                                           "Mixed",
                                           "Specialist Poultry",
                                           "Specialist Pigs", 
                                           "Grazing Livestock (Lowland)",
                                           "Grazing Livestock (Less Favoured Area)",
                                           "Dairy",
                                           "Horticulture",
                                           "General Cropping",
                                           "Cereals")),
                            labels = rev(c("All farms", 
                                           "Mixed",
                                           "Poultry",
                                           "Pigs", 
                                           "Lowland Grazing\nLivestock",
                                           "LFA Grazing\nLivestock",
                                           "Dairy",
                                           "Horticulture",
                                           "General cropping",
                                           "Cereals"))),
         category = factor(category, 
                           levels = c("bottom_25_percent",
                                      "middle_50_percent",
                                      "top_25_percent"),
                           labels = c("Bottom 25%", "Middle 50%", "Top 25%"))) |> 
  ggplot() +
  geom_point(aes(x = farm_type, y = value, colour = category, shape = category), size = 5) +
  scale_y_continuous(limits = c(0, 1.7)) +
  scale_colour_manual(values = af_colours(type = "sequential")) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(panel.grid.major.y = element_line(colour = "#cbcbcb"))
  
                               
                                
# save_graphic(econ_cht, "3.1.11b", "fbs economic performance")                 


# EXPERIMENT------------------------------------------------------------------------------

# Table 5a from https://www.gov.uk/government/statistics/farm-accounts-in-england-data-sets
 
margins <-  read_csv(file = "https://assets.publishing.service.gov.uk/media/65816325ed3c34000d3bfb0f/fae_table5a_2022_23.csv")                           
                                  
cropping_types <- c("Cereals", "General Cropping", "Mixed", "Horticulture")
livestock_types <- c("Dairy", "Grazing Livestock (Lowland)",
                     "Grazing Livestock (Less Favoured Area)",
                     "Specialist Pigs", "Specialist Poultry")
cht_data <- margins |> 
filter(stringr::str_detect(sda_status, "All"), stringr::str_detect(measure, "Farm Business Income")) |>
  mutate(chart_type = if_else(cost_centre == "Farm Business", "point", "bar"),
         farm_category = case_when(farm_type %in% cropping_types ~ "crops",
                                   farm_type %in% livestock_types ~ "livestock",
                                   TRUE ~ "all"),
         farm_type = forcats::fct_recode(farm_type, "Grazing Livestock (LFA)" = "Grazing Livestock (Less Favoured Area)")) |>
  select(survey_year, chart_type, farm_category, farm_type, cost_centre, value)                         

livestock_cht <- 
  ggplot() +
  geom_col(data = cht_data |> filter(chart_type == "bar", farm_category %in% c("livestock", "all")),
           aes(x = survey_year, y = value, fill = cost_centre)) +
  geom_point(data = cht_data |> filter(chart_type == "point", farm_category %in% c("livestock", "all")),
             aes(x = survey_year, y = value, colour = cost_centre), size = 3, shape = 21, fill = "black", stroke = 2, inherit.aes = FALSE) +
  scale_colour_manual(values = "white") +
  scale_fill_manual(values = af_colours()) +
  scale_x_discrete(limits = rev(unique(cht_data$survey_year))) +
  facet_wrap(vars(farm_type), labeller = label_wrap_gen(width = 15),
             ncol = 1, dir = "h", strip.position = "left") +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(x = NULL, y = NULL) +
    theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(size = 20, hjust = 1, angle = 0),
          strip.background = element_rect(fill = "white")) 

save_graphic(livestock_cht, "3.4.2z", "test fbs chart")


crops_cht <- 
  ggplot() +
  geom_col(data = cht_data |> filter(chart_type == "bar", farm_category %in% c("crops", "all")),
           aes(x = survey_year, y = value, fill = cost_centre)) +
  geom_point(data = cht_data |> filter(chart_type == "point", farm_category %in% c("crops", "all")),
             aes(x = survey_year, y = value, colour = cost_centre), size = 3, shape = 21, fill = "black", stroke = 2, inherit.aes = FALSE) +
  scale_colour_manual(values = "white") +
  scale_fill_manual(values = af_colours()) +
  scale_x_discrete(limits = rev(unique(cht_data$survey_year))) +
  facet_wrap(vars(farm_type), labeller = label_wrap_gen(width = 15),
             ncol = 1, dir = "h", strip.position = "left") +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website", horizontal = TRUE) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(size = 20, hjust = 1, angle = 0),
        strip.background = element_rect(fill = "white")) 


