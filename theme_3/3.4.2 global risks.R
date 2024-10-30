library(dplyr)
library(forcats)
library(ggplot2)
library(patchwork)
library(ukfsr)
library(aws.s3)
library(readr)

source(here::here("utils", "load-font.R"))

risks <- s3read_using(FUN = read_csv,
                    bucket = ukfsr::s3_bucket(),
                    object = "theme_3/input_data/top_10_risks.csv")

cols <- c("Economic" = "#12436D", "Environmental" = "#28A197", "Geopolitical" = "#801650", "Societal" = "#F46A25" , "Technological" = "#A285D1")

ords <- tibble(x = 1, y =-1, z = c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"))

l <- ords |> ggplot() +
  geom_text(aes(x = x, y = y, label = z),
            position = position_stack(vjust = 0.5),
            family = "GDS Transport Website", 
            size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.025))) +
  theme_void(base_family = "GDS Transport Website") 

r2 <- risks |> 
  filter(timescale == 2) |>
  mutate(event = fct_inorder(event)) |> 
  ggplot() +
  geom_col(aes(x = timescale, y = rank,  fill = category, group = rev(event)),
           colour = "white") +
  geom_text(aes(x = timescale, y = rank, label = event),
            position = position_stack(vjust = 0.5),
            family = "GDS Transport Website", 
            size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.025))) +
  scale_x_discrete(expand = expansion(mult = c(0,0.025))) +
  scale_fill_manual(values = cols) +
  labs(title = "2 years") +
  theme_void() +
  theme(plot.title = element_text(family = "GDS Transport Website",
                                  size = 18, face = "bold"),
        legend.text = element_text(family = "GDS Transport Website", size = 14), legend.title = element_blank())


r10 <- risks |> 
  filter(timescale == 10) |>
  mutate(event = fct_inorder(event)) |> 
  ggplot() +
  geom_col(aes(x = timescale, y = rank,  fill = category, group = rev(event)), 
           colour = "white") +
  geom_text(aes(x = timescale, y = rank, label = event),
            position = position_stack(vjust = 0.5),
            family = "GDS Transport Website",
            size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.025))) +
  scale_x_discrete(expand = expansion(mult = c(0,0.025))) +
  scale_fill_manual(values = cols) + 
  labs(title = "10 years") +
  theme_void() +
  theme(legend.position = "none", 
        plot.title = element_text(family = "GDS Transport Website",
                                  face = "bold",
                                  size = 18)) 

chart <-  l + r2 + r10 + plot_layout(guides = "collect", widths = c(1,4,4))

save_graphic(chart, "3.4.2", "top 10 risks")
